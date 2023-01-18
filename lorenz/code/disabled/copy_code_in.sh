#!/usr/bin/env bash
set -eu # strict config (non-zero returns and unset vars halt script)
source das_dir/das_tools.sh
source ./initialize_das.sh

copy_code_in(){
    target_dir=$1

    # Finding free temporary folder
    TMP_CODE_DIR(){ echo "$ARCH_DIR/tmp/das_copy${count}"; }
    count=1 # Code copy number
    while [[ -d $(TMP_CODE_DIR) ]]; do (( count++ )); done

    create_code_copy "$(TMP_CODE_DIR)"
    cp -r "$(TMP_CODE_DIR)" "$target_dir"
    # scp -r "$(TMP_CODE_DIR)/code" "$target_dir"
}

copy_code_in $@
exit $?