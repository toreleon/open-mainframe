//LOADPADB JOB (CCARD001),'IMS DB LOAD',CLASS=A,MSGCLASS=H,
//             MSGLEVEL=(1,1),TIME=1440,REGION=0M
//*------------------------------------------------------------*
//* IMS Database Load - Load CardDemo Account Database            *
//*------------------------------------------------------------*
//LOAD    EXEC PGM=DFSRRC00,REGION=0M,
//             PARM='DLI,LOADPADB,ACCTPSB'
//STEPLIB  DD DSN=IMS.SDFSRESL,DISP=SHR
//         DD DSN=AWS.M2.CARDDEMO.LOADLIB,DISP=SHR
//DFSRESLB DD DSN=IMS.SDFSRESL,DISP=SHR
//IMS      DD DSN=IMS.PSBLIB,DISP=SHR
//         DD DSN=IMS.DBDLIB,DISP=SHR
//IEFRDER  DD DUMMY
//SYSUDUMP DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//DFSUINP0 DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PS,DISP=SHR
//
