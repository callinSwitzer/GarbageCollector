
# get date
today=`date '+%Y_%m_%d__%H_%M_%S'`;
filename="CombinedApp_$today.pdf";

pdftk *Cover*.pdf \
*CV*.pdf \
*Research*.pdf \
*Teach*.pdf \
*Diversity*.pdf \
cat output \
$filename;

echo $filename;

