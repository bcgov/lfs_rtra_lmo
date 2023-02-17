DATA work.naics;
SET RTRAdata.LFS200005  (keep= ID AGE PROV ERTAB LFSSTAT NAICS_5 SYEAR);

/* Province B.C.*/
if PROV=59;

/* Age 15 or over */
if AGE >= 15;

/*Economic Region - British Columbia*/
length BC_Region $ 30;
IF   ERTAB IN ("5910") THEN BC_Region="Vancouver Island and Coast";
ELSE IF ERTAB IN ("5920") THEN BC_Region="Lower Mainland-Southwest";
ELSE IF ERTAB IN ("5930") THEN BC_Region="Thompson-Okanagan";
ELSE IF ERTAB IN ("5940") THEN BC_Region="Kootenay";
ELSE IF ERTAB IN ("5950") THEN BC_Region="Cariboo";
ELSE IF ERTAB IN ("5960") THEN BC_Region="North Coast";
ELSE IF ERTAB IN ("5970") THEN BC_Region="Nechako";
ELSE IF ERTAB IN ("5980") THEN BC_Region="Northeast";
ELSE BC_Region="Other";

/*Job Status employed in labour force*/
if LFSSTAT IN (1,2);

run;

%RTRAFreq(
     InputDataset=work.naics,
     OutputName=emp0005naics,
     ClassVarList= SYEAR BC_Region NAICS_5,
     UserWeight=FINALWT);

