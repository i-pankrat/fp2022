#!/usr/bin/env sh

git remote | grep upstream
if [ $? -ne 0 ]
then
  git remote add upstream https://github.com/Kakadu/fp2022.git
  git fetch upstream master
fi

echo ""
CHANGES=`git diff-tree $(git merge-base upstream/master $1)..$1 | rev | cut -f 1 | rev`

set answer=""
for dir in $CHANGES
do
  #echo $dir
  if [ -d "$dir" ]; then
    if [ "$dir" != ".github" ]; then
      answer="$answer\n$dir"
      #echo dir answer="$answer"
    fi
  else
    :
  fi
done

topnames=`echo $answer | sed '/^$/d' | uniq`
rez=`echo $topnames | wc -l`
if [ "$rez" = "1" ]; then
  echo "latest=$topnames"
  exit 0
else
  echo "Too many cancdidates ($rez) to be a last solution"
  echo "'$topnames'"
  exit 1
fi
