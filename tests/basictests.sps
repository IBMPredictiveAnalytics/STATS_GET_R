stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata"
/get dataframe=ovarian dataset=ovarian.

dataset close all.
new file.

stats get r file=*
/get dataframe=ovarian dataset=ovarian.

* Different environment.
stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata" clearenv= yes
/get dataframe=ovarian dataset=ovarian.

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata" clearenv= no
/get dataframe=ovarian dataset=ovarian.
stats get r file=* clearenv= no
/get dataframe=ovarian dataset=ovarian2.
stats get r file=* clearenv= yes
/get dataframe=ovarian dataset=ovarian3.

* with factors.
stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/simpledf.Rdata"
/get dataframe=df dataset=df.

* display information.
stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/simpledf.Rdata".

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/simpledf.Rdata"
/info classes=all.

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata"
/info classes=all.

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata"
/info classes=list data.frame.

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata"
/info classes=list data.frame names=".*i" ".*v" .

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/ovarianetc.Rdata"
/info classes=list data.frame names=".*v" ".*i"  .
stats get r  file="c:/cc/misc2/extensions/r/stats_get_r/tests/misc.Rdata"
/info classes=..

stats get r  file="c:/cc/misc2/extensions/r/stats_get_r/tests/misc.Rdata"
/get dataframe=modelerData dataset=modeler.

stats get r  file="c:/cc/misc2/extensions/r/stats_get_r/tests/dfwnonfactorstring.Rdata"
/get dataframe=df2, dataset=nonfactorstring.

stats get r  file="c:/cc/misc2/extensions/r/stats_get_r/tests/weirddf.Rdata"
/get dataframe=weirddf, dataset=weirddf.

stats get r file=*.

STATS GET R FILE=* /INFO ALLDATA=YES.

STATS GET R DATA=ppine PACKAGE=systemfit.

stats get r file="c:/cc/misc2/extensions/r/stats_get_r/tests/separateVariables.Rdata"
/info classes=..

STATS GET R FILE="C:\cc\misc2\extensions\R\STATS_GET_R\tests\separateVariables.Rdata" CLEARENV=NO       
/GET DATASET=normal SERIES=xnormal xpois.
* short series.
STATS GET R FILE="C:\cc\misc2\extensions\R\STATS_GET_R\tests\separateVariables.Rdata" CLEARENV=NO       
/GET DATASET=normal SERIES=xnormal xpois xpois10.
