DATA work.LFS_Retirement;
SET RTRAdata.LFS202125 (keep= ID SYEAR AGE WHYLEFT NOC_5);

/* Age 15 or over */
if AGE >= 15;

If WHYLEFT = 18; /* Reason to left the previous job = Retired */

/*Second half of the NOCS (missing in other half)*/

if NOC_5 > 49999;


run;

%RTRAFreq(
     InputDataset=work.LFS_Retirement,
     OutputName=retire2125p2,
     ClassVarList= SYEAR NOC_5 AGE,
     UserWeight=FINALWT);
run;
