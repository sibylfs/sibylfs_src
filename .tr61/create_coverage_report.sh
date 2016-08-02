#!/bin/bash -e

# this script uses bisect to generate an html report from a directory
# containing bisect files
#
# Usage:
#     myself directory_with_posix_results
#
# or for quiet mode
#     myself -q directory_with_posix_results

if [[ ! $@ || $1 == "-h" || $1 == "--help" ]]
then
            echo "Usage: create_coverage_report.sh ... [DIR]

      OPTIONS:
      -h,--help     show a message help
      "
            exit 0;
fi;

results_dir=`readlink -f $1`
coverage_report_dir=coverage_report_$(date -I)_$(basename $results_dir)
final_bisect_file=$coverage_report_dir/final_bisect.out

echo "creating report dir $coverage_report_dir"
mkdir -p $coverage_report_dir

first_fragment=`find $results_dir -wholename *results/bisect0001.out | head -1`
# we initialize the final_bisect_file with the first bisect fragment we find
cp $first_fragment $final_bisect_file

echo "merging fragments in $final_bisect_file"
for bisect_fragment in `find $results_dir -wholename *results/bisect*`;
do
    # we merge the final_bisect_file with all the bisect fragments
    bisect-report -bisect $final_bisect_file $final_bisect_file $bisect_fragment
done;

initial_dir=`pwd .`
cd build_spec
echo "creating html report in $coverage_report_dir"
bisect-report -html $initial_dir/$coverage_report_dir $initial_dir/$final_bisect_file

echo "done"
echo "to see the html:"
echo "firefox $initial_dir/$coverage_report_dir/index.html"
