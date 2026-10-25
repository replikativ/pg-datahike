# Source from a regression shell wrapper after defining script_dir.
# NUL-delimited values preserve paths without evaluating shell config text.
reference_config_file="$(mktemp)"
if ! bb "${script_dir}/reference.clj" env > "${reference_config_file}"; then
  rm -f "${reference_config_file}"
  exit 2
fi
while IFS= read -r -d '' reference_key && IFS= read -r -d '' reference_value; do
  export "${reference_key}=${reference_value}"
done < "${reference_config_file}"
rm -f "${reference_config_file}"
unset reference_config_file reference_key reference_value

# Bound administrative/preflight statements and their owned client processes.
reference_psql() {
  bb "${script_dir}/reference.clj" psql "$@"
}
