#!/bin/bash

## Will split comma-separated data into columns.


USAGE="""

    split_data_to_columns.bash    filename    

"""

infile=${1}

var="$(head -n 1 ${infile})"
res="${var//[^,]}"
ncol="${#res}"
ncol=$((ncol+1))

echo "ncol is ${ncol}."

i=1
while [ "${i}" -le "${ncol}" ] ; do
	cut -d ',' -f ${i} ${infile} > ${infile}.col_${i}
	i=$((i+1))
done
