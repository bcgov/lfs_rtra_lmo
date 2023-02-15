DATA work.LFS_Retirement;
SET RTRAdata.LFS201115 (keep= ID SYEAR AGE WHYLEFT NOC_5);

/* Age 15 or over */
if AGE >= 15;

If WHYLEFT = 18; /* Reason to left the previous job = Retired */

/*First half of the NOCS plus missing*/

if NOC_5 < 50000;
if missing(NOC_5) then NOC_5="missing";

run;

%RTRAFreq(
     InputDataset=work.LFS_Retirement,
     OutputName=retire1115p1,
     ClassVarList= SYEAR NOC_5 AGE,
     UserWeight=FINALWT);
run;
