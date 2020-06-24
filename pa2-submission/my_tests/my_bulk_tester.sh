programs_dir=../../../Resources/programs/examples/*.cl
if [ $# -ne 0 ]
  then
    programs_dir=$1
fi

for filename in $programs_dir; do
    ./my_tester.sh $filename
    done