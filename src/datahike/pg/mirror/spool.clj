(ns datahike.pg.mirror.spool
  "Ephemeral pgoutput files. WAL feedback never depends on these files."
  (:import [java.io BufferedInputStream BufferedOutputStream DataInputStream DataOutputStream RandomAccessFile]
           [java.nio ByteBuffer]
           [java.nio.file Files LinkOption OpenOption Path StandardOpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn store [path limit]
  {:directory (.toPath (java.io.File. (str path ".mirror-spool")))
   :limit limit :bytes (atom 0)})

(defn prepare! [{:keys [^Path directory]}]
  (let [nofollow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])]
    (when-not (Files/exists directory nofollow)
      (let [attributes (when (contains? (.supportedFileAttributeViews (.getFileSystem directory)) "posix")
                         [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rwx------"))])]
        (Files/createDirectory directory (into-array FileAttribute attributes))))
    (when-not (Files/isDirectory directory nofollow)
      (throw (ex-info "Mirror spool path must be a directory" {:path (str directory)})))
    ;; The store lock excludes other mirror workers. Files from a process
    ;; crash can be thrown away because the upstream slot still owns the WAL.
    (with-open [entries (Files/newDirectoryStream directory "pgdh-*.pgoutput")]
      (doseq [^Path entry entries
              :when (and (re-matches #"pgdh-[0-9]+\.pgoutput" (str (.getFileName entry)))
                         (Files/isRegularFile entry nofollow))]
        (Files/deleteIfExists entry)))))

(defn session [spool-store transaction-limit]
  {:store spool-store :transaction-limit transaction-limit
   :transactions (volatile! {}) :writer (volatile! nil)})

(defn- checked-transaction [{:keys [transactions]} xid]
  (or (get @transactions xid)
      (throw (ex-info "Unknown streamed transaction" {:xid xid}))))

(defn begin-segment! [{:keys [transactions writer] :as session} xid first?]
  (when (or @writer (not= first? (not (contains? @transactions xid))))
    (throw (ex-info "Unexpected pgoutput stream start" {:xid xid :first? first?})))
  (let [entry (if first?
                (let [file (Files/createTempFile ^Path (get-in session [:store :directory]) "pgdh-" ".pgoutput"
                                                 (make-array FileAttribute 0))
                      entry {:path file :size 0 :subtransactions {}}]
                  (vswap! transactions assoc xid entry)
                  entry)
                (checked-transaction session xid))
        output (Files/newOutputStream ^Path (:path entry) (into-array OpenOption [StandardOpenOption/APPEND]))]
    (vreset! writer (DataOutputStream. (BufferedOutputStream. output)))))

(defn end-segment! [{:keys [writer]}]
  (when-let [^DataOutputStream handle @writer]
    (vreset! writer nil)
    (.close handle)))

(defn append! [{:keys [store transaction-limit transactions writer] :as session} xid subxid ^ByteBuffer frame]
  (let [length (.remaining frame)
        added (+ 4 length)
        {:keys [size subtransactions] :as entry} (checked-transaction session xid)
        next-size (+ size added)
        ^DataOutputStream handle @writer]
    (when (> next-size transaction-limit)
      (throw (ex-info "Transaction exceeds the configured byte limit" {:xid xid :limit transaction-limit})))
    (swap! (:bytes store)
           (fn [used]
             (let [total (+ used added)]
               (when (> total (:limit store))
                 (throw (ex-info "Streamed transactions exceed the configured spool limit"
                                 {:reason :stream-spool-limit :limit (:limit store) :xid xid})))
               total)))
    (vswap! transactions assoc xid
            (cond-> (assoc entry :size next-size)
              (and subxid (not= xid subxid) (not (contains? subtransactions subxid)))
              (assoc-in [:subtransactions subxid] size)))
    (.writeInt handle length)
    (if (.hasArray frame)
      (.write handle (.array frame) (+ (.arrayOffset frame) (.position frame)) length)
      (let [bytes (byte-array length)]
        (.get frame bytes)
        (.write handle bytes)))))

(defn discard! [{:keys [store transactions]} xid]
  (when-let [{:keys [^Path path size]} (get @transactions xid)]
    (Files/deleteIfExists path)
    (vswap! transactions dissoc xid)
    (swap! (:bytes store) - size)))

(defn abort! [{:keys [store transactions] :as session} xid subxid]
  (if (= xid subxid)
    (discard! session xid)
    (let [{:keys [^Path path size subtransactions] :as entry} (get @transactions xid)]
      (when-let [offset (get subtransactions subxid)]
        (with-open [handle (RandomAccessFile. (.toFile path) "rw")]
          (.setLength handle (long offset)))
        (vswap! transactions assoc xid
                (assoc entry :size offset :subtransactions
                       (into {} (filter (fn [[_ start]] (< start offset))) subtransactions)))
        (swap! (:bytes store) - (- size offset))))))

(defn frames [session xid]
  (let [{:keys [^Path path size]} (checked-transaction session xid)]
    ;; Keep only one decoded frame in memory. Callers may reduce the file
    ;; again inside a retried apply without holding onto a lazy sequence.
    (reify clojure.lang.IReduceInit
      (reduce [_ rf init]
        (with-open [input (DataInputStream. (BufferedInputStream. (Files/newInputStream path (make-array OpenOption 0))))]
          (loop [acc init remaining (long size)]
            (if (zero? remaining)
              acc
              (let [length (.readInt input)]
                (when-not (<= 1 length (- remaining 4))
                  (throw (ex-info "Invalid pgoutput spool frame length" {:xid xid :length length})))
                (let [data (byte-array length)
                      _ (.readFully input data)
                      next (rf acc (ByteBuffer/wrap data))]
                  (if (reduced? next)
                    @next
                    (recur next (- remaining 4 length))))))))))))

(defn byte-size [session xid]
  (:size (checked-transaction session xid)))

(defn close! [{:keys [transactions] :as session}]
  (try (end-segment! session)
       (finally (doseq [xid (keys @transactions)] (discard! session xid)))))
