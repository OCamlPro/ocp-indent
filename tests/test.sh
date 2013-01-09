#!/bin/bash -ue

ROOT=$(git rev-parse --show-toplevel)
OCP_INDENT="$ROOT"/ocp-indent
cd $ROOT/tests

UPDATE=
GIT=
SHOW=
SHOWCMD=

while [ $# -gt 0 ]; do
    case "$1" in
        --update|-u)
            UPDATE=1
            ;;
        --git-update)
            if ! git diff --exit-code -- . >/dev/null; then
                echo -e "\e[1mWarning:\e[m unstaged changes in tests/"
                echo "You may want to do 'git checkout -- tests/' or"\
                     "'git add -u -- tests/' first."
                exit 1
            fi
            UPDATE=1
            GIT="git "
            ;;
        --ocp-indent)
            if [ $# -le 1 ]; then echo "Error: $1 needs an argument"; exit 1; fi
            shift;
            OCP_INDENT=$1
            ;;
        --show)
            SHOW=1
            ;;
        --meld)
            SHOW=1
            SHOWCMD="meld"
            ;;
        *)
            cat <<EOF >/dev/stderr
Usage:
  -u --update         update the files according to the current results
  --git-update        update the files and state the changes in git
  --ocp-indent <prg>  use this ocp-indent exe
  --show              show a diff of changed results
  --meld              show progressions/regressions using meld
EOF
            exit 1
    esac
    shift
done

TMP=$(mktemp -d /tmp/ocp-indent-test.XXXXX)
trap "rm -rf /tmp/ocp-indent-${TMP#/tmp/ocp-indent-}" EXIT

ocp-indent() {
    [ $# -eq 1 ]
    opts=$(cat $1.opts 2>/dev/null || true)
    "$OCP_INDENT" $opts "$1" >$TMP/$(basename $1)
}

if [ -n "$GIT" ]; then
    PASSING=($(git ls-files 'passing/*.ml'))
    FAILING=($(git ls-files 'failing/*.ml'))
else
    PASSING=(passing/*.ml)
    FAILING=(failing/*.ml)
fi
CHANGES=()


for f in ${PASSING[@]}; do
    name=$(basename $f .ml)
    ocp-indent $f
    if diff -q $f $TMP/$name.ml >/dev/null; then
        printf "%-12s\t\e[32m[PASSED]\e[m\n" $name
    else
        printf "%-12s\t\e[31m[FAILED]\e[m \e[41m\e[30m[REGRESSION]\e[m\n" $name
        if [ -n "$UPDATE" ]; then
            $GIT mv -f $f* failing/
            f=failing/${f#passing/}
            cp $TMP/$name.ml failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$name.ml; fi
        fi
        CHANGES+=($f)
    fi
done

for f in ${FAILING[@]}; do
    name=$(basename $f .ml)
    ocp-indent $f
    if diff -q $f $TMP/$name.ml >/dev/null; then
        printf "%-12s\t\e[32m[PASSED]\e[m \e[42m\e[30m[PROGRESSION]\e[m\n" $name
        if [ -n "$UPDATE" ]; then
            $GIT mv -f $f* passing/
            $GIT rm -f failing-output/$name.ml
        fi
    elif [ ! -e failing-output/$name.ml ]; then
        printf "%-12s\t\e[33m[FAILED]\e[m \e[43m\e[30m[NEW]\e[m\n" $name
        cp $TMP/$name.ml failing-output/
        if [ -n "$GIT" ]; then $GIT add failing-output/$name.ml; fi
    elif diff -q $TMP/$name.ml failing-output/$name.ml >/dev/null; then
        printf "%-12s\t\e[33m[FAILED]\e[m\n" $name
    else
        refcount=$(diff -y --suppress-common-lines $f failing-output/$name.ml \
            |wc -l)
        curcount=$(diff -y --suppress-common-lines $f $TMP/$name.ml \
            |wc -l)
        progress=$((refcount - curcount))
        printf "%-12s\t\e[33m[FAILED]\e[m \e[%dm\e[30m[CHANGE: %+d]\e[m\n" \
            $name \
            $(if [ $progress -gt 0 ]; then echo 42; \
              elif [ $progress -eq 0 ]; then echo 43; \
              else echo 41; fi) \
            $progress
        if [ -n "$UPDATE" ]; then
            cp $TMP/$name.ml failing-output/
            if [ -n "$GIT" ]; then $GIT add failing-output/$name.ml; fi
        fi
        CHANGES+=($f)
    fi
done

if [ -n "$SHOW" ] && [ ${#CHANGES[@]} -gt 0 ]; then
    if [ -z "$SHOWCMD" ]; then
        for f in ${CHANGES[@]}; do
            echo
            printf "\e[1m=== Showing differences in %s ===\e[m\n" $f
            colordiff -y $f $TMP/$(basename $f) || true
        done
    else
        echo
        echo "Meld view:"
        echo "[reference] [current status] [new result]"
        echo "You can update reference and current status from meld"
        cmd=(meld)
        for f in ${CHANGES[@]}; do
            cur=failing-output/$(basename $f)
            if ! [ -e $cur ]; then cur=; fi
            cmd+=(--diff $f $cur $TMP/$(basename $f))
        done
        ${cmd[*]}
    fi
elif [ -n "$SHOW" ]; then
    echo
    echo "No changes to show. To check the current failures use for example:"
    echo "  meld tests/failing tests/failing-output"
fi

exit ${#CHANGES[@]}
