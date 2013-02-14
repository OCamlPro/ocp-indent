#!/bin/bash -ue
shopt -s nullglob

ROOT=$(git rev-parse --show-toplevel)
OCP_INDENT="$ROOT"/ocp-indent
cd $ROOT/tests

UPDATE=
GIT=
SHOW=
SHOWCMD=
HTML=

usegit() {
    printf "%-12s\t\e[34mgit %s\e[m\n" "" "$*";
    git "$@";
}

is_file_on_git() {
    [ $# -eq 1 ]; f=$1
    git ls-files $f --error-unmatch >/dev/null 2>&1
}

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
            GIT="usegit "
            HTML=1
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
        --html)
            HTML=1
            ;;
        *)
            cat <<EOF >/dev/stderr
Usage:
  -u --update         update the files according to the current results
  --git-update        update the files and state the changes in git
  --ocp-indent <prg>  use this ocp-indent exe
  --show              show a diff of changed results
  --meld              show progressions/regressions using meld
  --html              generate an html page showing the diff of failing tests
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
    "$OCP_INDENT" $opts "$1" >$TMP/$(basename $1) 2>&1 || true
}

PASSING=("")
FAILING=("")
if [ -n "$GIT" ]; then
    PASSING+=($(git ls-files 'passing/*.ml'))
    FAILING+=($(git ls-files 'failing/*.ml'))
else
    PASSING+=(passing/*.ml)
    FAILING+=(failing/*.ml)
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
        if [ -n "$GIT" ] && ! is_file_on_git failing-output/$name.ml; then
            $GIT add failing-output/$name.ml; fi
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
            diff -W 130 -ty  $f $TMP/$(basename $f) \
                | awk '/^.{64}[^ ].*/ { printf "[31m%s[m\n",$0; next } 1' \
                || true
        done
    else
        echo
        echo "Meld view:"
        echo "[reference] [new result] [registered]"
        echo "You can update reference and registered status from meld"
        cmd=(meld)
        for f in ${CHANGES[@]}; do
            cur=failing-output/$(basename $f)
            if ! [ -e $cur ]; then cur=; fi
            cmd+=(--diff $f $TMP/$(basename $f) $cur)
        done
        ${cmd[*]}
    fi
elif [ -n "$SHOW" ]; then
    echo
    echo "No changes to show. To check the current failures use for example:"
    echo "  meld tests/failing tests/failing-output"
fi

if [ -n "$HTML" ]; then
    VERSION=$($OCP_INDENT --version | awk '{ print $NF; exit }')
    COMMITS_SINCE=$(git log --oneline $VERSION.. 2>/dev/null || true)
    if [ -n "$COMMITS_SINCE" ]; then
        VERSION="$VERSION+$((1+$(wc -l <<<"$COMMITS_SINCE")))"
    fi
    VERSION_STRING="$VERSION ($(date +%F))"
    echo
    echo -n "Generating summary of failures tests/failing.html..."
    cat <<EOF > failing.html
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
 "http://www.w3.org/TR/REC-html40/loose.dtd">
<html>
<head>
    <title>Failing tests, ocp-indent version $VERSION_STRING</title>
    <style>
      BODY { font-family: monospace; }
      TABLE { border-collapse: collapse; border-spacing: 0px; margin: auto; }
      TD { padding: 0; }
      TD.linenum {
         color: #909090;
         text-align: right;
         vertical-align: top;
         font-weight: bold;
         border-right: 1px solid black;
         border-left: 1px solid black;
      }
      TD.added { background-color: #DDDDFF; }
      TD.modified { background-color: #BBFFBB; }
      TD.removed { background-color: #FFCCCC; }
      TD.normal { background-color: #FFFFE1; }
    </style>
</head>
<body>
<h1>Failing tests, ocp-indent version $VERSION_STRING</h1>
<p>Left is expected result, right shows actual indentation by ocp-indent</p>
EOF
    complete_success="1"
    for f in $(git ls-files 'failing/*.ml'); do
        complete_success=
        $ROOT/tools/diff2html --no-header "$f" "failing-output/${f#failing/}" \
            >>failing.html 2>/dev/null || true
        echo -n "."
    done
    if [ -n "$complete_success" ]; then
        echo "<p>All tests pass: no currently known bugs.</p>" >>failing.html
    fi
    cat <<EOF >>failing.html
</body>
</html>
EOF

    echo " done"
    if [ -n "$GIT" ]; then $GIT add failing.html; fi
fi

exit ${#CHANGES[@]}
