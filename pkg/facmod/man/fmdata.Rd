\name{fmdata}
\alias{fmdata}
\docType{data}
\title{
fmdata
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(fmdata)}
\format{
  The format is:
Formal class 'fmdsCube.CRSP' [package ".GlobalEnv"] with 11 slots
  ..@ interp   : chr(0) 
  ..@ data     : num [1:60, 1:33, 1:562] 40 32.7 27.9 33.4 30.6 ...
  .. ..- attr(*, "dimnames")=List of 3
  .. .. ..$ : chr [1:60] "1999-01-29" "1999-02-26" "1999-03-31" "1999-04-30" ...
  .. .. ..$ : chr [1:33] "PRICE" "RETURN" "VOLUME" "SHARES.OUT" ...
  .. .. ..$ : chr [1:562] "10032" "10078" "10085" "10104" ...
  ..@ labels   :List of 562
  .. ..$ 10032: chr "PLXS"
  .. ..$ 10078: chr "SUNW"
  .. ..$ 10085: chr "UCI"
  .. ..$ 10104: chr "ORCL"
  .. ..$ 10107: chr "MSFT"
  .. ..$ 10108: chr "SDS"
  .. ..$ 10119: chr "TW"
  .. ..$ 10137: chr "AYE"
  .. ..$ 10138: chr "TROW"
  .. ..$ 10145: chr "HON"
  .. ..$ 10147: chr "EMC"
  .. ..$ 10182: chr "TECD"
  .. ..$ 10211: chr "WES"
  .. ..$ 10225: chr "FO"
  .. ..$ 10272: chr "TKLC"
  .. ..$ 10290: chr "HGIC"
  .. ..$ 10299: chr "LLTC"
  .. ..$ 10302: chr "CY"
  .. ..$ 10304: chr "AMFI"
  .. ..$ 10375: chr "TCB"
  .. ..$ 10397: chr "WERN"
  .. ..$ 10401: chr "T"
  .. ..$ 10421: chr "SKYW"
  .. ..$ 10560: chr "ABY"
  .. ..$ 10562: chr "SOV"
  .. ..$ 10629: chr "FNB"
  .. ..$ 10638: chr "HNBC"
  .. ..$ 10691: chr "ASCL"
  .. ..$ 10696: chr "FISV"
  .. ..$ 10725: chr "CF"
  .. ..$ 10777: chr "FCNCA"
  .. ..$ 10812: chr "HTLD"
  .. ..$ 10825: chr "TSFG"
  .. ..$ 10874: chr "BC"
  .. ..$ 10890: chr "UIS"
  .. ..$ 10906: chr "BNK"
  .. ..$ 10909: chr "CERN"
  .. ..$ 10932: chr "WBS"
  .. ..$ 10933: chr "MKL"
  .. ..$ 10942: chr "CMVT"
  .. ..$ 11018: chr "FBP"
  .. ..$ 11081: chr "DELL"
  .. ..$ 11154: chr "PLAB"
  .. ..$ 11293: chr "WSBC"
  .. ..$ 11353: chr "TOPP"
  .. ..$ 11369: chr "UBSI"
  .. ..$ 11371: chr "FHCC"
  .. ..$ 11382: chr "SKS"
  .. ..$ 11384: chr "WLM"
  .. ..$ 11389: chr "FCTR"
  .. ..$ 11403: chr "CDN"
  .. ..$ 11404: chr "ED"
  .. ..$ 11406: chr "SUR"
  .. ..$ 11415: chr "BRL"
  .. ..$ 11478: chr "BRY"
  .. ..$ 11509: chr "HRH"
  .. ..$ 11531: chr "FILE"
  .. ..$ 11533: chr "FIC"
  .. ..$ 11547: chr "CNMD"
  .. ..$ 11600: chr "XRAY"
  .. ..$ 11618: chr "FAST"
  .. ..$ 11634: chr "RBNC"
  .. ..$ 11674: chr "DTE"
  .. ..$ 11691: chr "ALEX"
  .. ..$ 11707: chr "CBM"
  .. ..$ 11731: chr "AIN"
  .. ..$ 11762: chr "ETN"
  .. ..$ 11786: chr "SIVB"
  .. ..$ 11823: chr "PBKS"
  .. ..$ 11850: chr "XOM"
  .. ..$ 11891: chr "MGG"
  .. ..$ 11896: chr "MXIM"
  .. ..$ 11927: chr "RESP"
  .. ..$ 11955: chr "WMI"
  .. ..$ 11970: chr "APCC"
  .. ..$ 11976: chr "BMC"
  .. ..$ 11992: chr "CHFC"
  .. ..$ 11995: chr "COKE"
  .. ..$ 12008: chr "DY"
  .. ..$ 12022: chr "GLYT"
  .. ..$ 12036: chr "GMT"
  .. ..$ 12052: chr "GD"
  .. ..$ 12067: chr "NVLS"
  .. ..$ 12068: chr "ONB"
  .. ..$ 12074: chr "PDE"
  .. ..$ 12169: chr "DRL"
  .. ..$ 12265: chr "EYE"
  .. ..$ 12490: chr "IBM"
  .. ..$ 12503: chr "NAV"
  .. ..$ 12546: chr "N"
  .. ..$ 12570: chr "ITT"
  .. ..$ 13056: chr "AM"
  .. ..$ 13100: chr "MAY"
  .. ..$ 13119: chr "MYG"
  .. ..$ 13303: chr "NL"
  .. ..$ 13507: chr "ANAT"
  .. ..$ 13610: chr "OLN"
  .. ..$ 13688: chr "PCG"
  .. ..$ 13821: chr "PGL"
  .. .. [list output truncated]
  ..@ info     :'data.frame':	1019 obs. of  12 variables:
  .. ..$ DATE         : Date[1:1019], format: "1999-01-29" ...
  .. ..$ PERMNO       : chr [1:1019] "10032" "10032" "10032" "10078" ...
  .. ..$ CUSIP        : chr [1:1019] "72913210" "72913210" "72913210" "86681010" ...
  .. ..$ COMPANY.NAME : chr [1:1019] "PLEXUS CORP" "PLEXUS CORP" "PLEXUS CORP" "SUN MICROSYSTEMS INC" ...
  .. ..$ TICKER       : chr [1:1019] "PLXS" "PLXS" "PLXS" "SUNW" ...
  .. ..$ EXCHANGE     : chr [1:1019] "3" "3" "3" "3" ...
  .. ..$ ZLIST        : chr [1:1019] "25" "25" "25" "21" ...
  .. ..$ SHARE.CLASS  : chr [1:1019] "11" "11" "11" "11" ...
  .. ..$ SIC          : chr [1:1019] "3670" "3670" "3670" "3570" ...
  .. ..$ GICS         : chr [1:1019] "20104010" "45203010" "45203020" "45202010" ...
  .. ..$ SIC.INDUSTRY : chr [1:1019] "Manufacturing" "Manufacturing" "Manufacturing" "Manufacturing" ...
  .. ..$ GICS.INDUSTRY: chr [1:1019] "Capital Goods" "Technology Hardware & Equipment" "Technology Hardware & Equipment" "Technology Hardware & Equipment" ...
  ..@ auxData  :List of 6
  .. ..$ T90RET     :List of 11
  .. .. ..$ data             : num [1:60] 0.0036 0.003 0.0043 0.0036 0.0035 0.0045 0.0041 0.0042 0.0046 0.0039 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  .. ..$ T30RET     :List of 11
  .. .. ..$ data             : num [1:60] 0.0035 0.0034 0.0041 0.0038 0.0035 0.0041 0.0037 0.0039 0.0037 0.0036 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  .. ..$ CPIRET     :List of 11
  .. .. ..$ data             : num [1:60] 0.0024 0.0012 0.003 0.0073 0 0 0.003 0.0024 0.0048 0.0018 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  .. ..$ SP500RET   :List of 11
  .. .. ..$ data             : num [1:60] 0.041 -0.0323 0.0388 0.0379 -0.025 0.0544 -0.032 -0.0063 -0.0286 0.0625 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  .. ..$ MKT.VALWRET:List of 11
  .. .. ..$ data             : num [1:60] 0.0384 -0.0381 0.0381 0.0489 -0.0207 0.0511 -0.0305 -0.01 -0.0226 0.0621 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  .. ..$ MKT.EQUWRET:List of 11
  .. .. ..$ data             : num [1:60] 0.0647 -0.0421 -0.0013 0.0839 0.0251 0.0385 0.0072 -0.0306 -0.0142 -0.0013 ...
  .. .. ..$ positions        :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int [1:60] 14273 14301 14334 14364 14392 14425 14455 14487 14517 14546 ...
  .. .. .. .. ..$ : int [1:60] 0 0 0 0 0 0 0 0 0 0 ...
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ start.position   :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ end.position     :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ future.positions :List of 5
  .. .. .. ..$ .Data        :List of 2
  .. .. .. .. ..$ : int(0) 
  .. .. .. .. ..$ : int(0) 
  .. .. .. ..$ .Data.names  : chr [1:2] "julian.day" "milliseconds"
  .. .. .. ..$ .Data.classes: chr [1:2] "integer" "integer"
  .. .. .. ..$ format       : chr "%02m/%02d/%Y %02H:%02M:%02S.%03N"
  .. .. .. ..$ time.zone    : chr "GMT"
  .. .. ..$ units            : chr(0) 
  .. .. ..$ title            : chr(0) 
  .. .. ..$ documentation    : chr(0) 
  .. .. ..$ attributes       : symbol attributes
  .. .. ..$ fiscal.year.start: num 1
  .. .. ..$ type             : chr(0) 
  ..@ datefmt  : chr "%Y-%m-%d"
  ..@ matchfmt : chr "%Y-%m"
  ..@ contents : chr [1:45, 1:2] "PRICE" "RETURN" "VOLUME" "SHARES.OUT" ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : NULL
  ..@ index    :List of 2
  .. ..$ datevar : chr "DATE"
  .. ..$ assetvar: chr "PERMNO"
  ..@ labelvar : chr "TICKER"
  ..@ buildDate: Date[1:1], format: "2011-05-16"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fmdata)
## maybe str(fmdata) ; plot(fmdata) ...
}
\keyword{datasets}
