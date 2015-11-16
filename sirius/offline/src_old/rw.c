#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>
#include	<fcntl.h>						/* Header for open/read/write */ 
#include	<string.h>
#include	<ctype.h>
#include	<time.h>

int norw1dim(FILE *fp, char *comment, int *pxdim, float ax[8192], float cal[6])
{
	char line[128];
	int i;
	int ical = 3;
	int xdim;
	extern int datetime(char *);
	char  dattim[20];
	if(cal[0] + cal[1] + cal[2] == 0){
		cal[0] = 0.;
		cal[1] = 1.;
		cal[2] = 0.;
	}
	xdim = *pxdim;
	datetime(dattim);
	/* ******************************************** */
	/* Singles spectrum with header written to disk */
	/* ******************************************** */
   if(fp == NULL){
		printf("Could not open file for writing \n");
		return(-1);
   }
	else {
		fprintf(fp, "!FILE=Disk \n");
		fprintf(fp, "!KIND=Spectrum \n");
		fprintf(fp, "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n");
		fprintf(fp, "!EXPERIMENT=Offline \n");
		fprintf(fp, "!COMMENT=%s\n", comment);                             
		fprintf(fp, "!TIME=%s\n",dattim);
		fprintf(fp, "!CALIBRATION EkeV=%1d",ical);
		for (i = 0; i < ical; i++){
			fprintf(fp,"%13e,",cal[i]);
		}
		fprintf(fp, "\n!PRECISION=16\n");
		fprintf(fp, "!DIMENSION=1,0:%4d\n",xdim-1);
		fprintf(fp, "!CHANNEL=(0:%4d)\n",xdim-1);
		for (i = 0; i < xdim; i++){
		   fprintf(fp, "%e ", ax[i]);
      }
		fprintf(fp, "\n!IDEND=\n");
   }
	return 0;
}

int norw2dim(FILE *fp, char *comment, int *pxdim, int *pydim, float mx[4096][512], float cal[6])
{
	char line[128];
	int i, j;
	int ical = 6;
	int xdim, ydim;
	extern int datetime(char *);
	char   dattim[20];
	datetime(dattim);
	xdim = *pxdim;
	ydim = *pydim;
	if(cal[0] + cal[1] + cal[2] == 0){
		cal[0] = 0.;
		cal[1] = 1.;
		cal[2] = 0.;
	}
	if(cal[3] + cal[4] + cal[5] == 0){
		cal[3] = 0.;
		cal[4] = 1.;
		cal[5] = 0.;
	}
	/* ********************************** */
	/* Matrix with header written to disk */
	/* ********************************** */
   if(fp == NULL){
		printf("Could not open file for writing \n");
		return(-1);
   }
	else {
		fprintf(fp, "!FILE=Disk \n");
		fprintf(fp, "!KIND=Matrix \n");
		fprintf(fp, "!LABORATORY=Oslo Cyclotron Laboratory (OCL) \n");
		fprintf(fp, "!EXPERIMENT=Offline \n");
		fprintf(fp, "!COMMENT=%s\n", comment);
		fprintf(fp, "!TIME=%s\n",dattim);
		fprintf(fp, "!CALIBRATION EkeV=%1d",ical);
		for (i = 0; i < ical; i++){
			fprintf(fp,"%13e,",cal[i]);
		}
		fprintf(fp, "\n!PRECISION=16\n");
		fprintf(fp, "!DIMENSION=2,0:%4d,0:%4d\n",xdim-1,ydim-1);
		fprintf(fp, "!CHANNEL=(0:%4d,0:%4d)\n",xdim-1,ydim-1);
		for (j = 0; j < ydim; j++){
			for (i = 0; i < xdim; i++){
				fprintf(fp, "%e ", mx[i][j]);
				if(i == (int)((i/(xdim-1))*(xdim-1)) && i > 0)fprintf(fp, "\n");
			}
		}
		fprintf(fp, "!IDEND=\n");
   }
	return 0;
}


int norr1dim(FILE *fp, char *comment, int *pxdim, float ax[8192], float cal[6])
{
	char	line[128];
	int		i;
	int		xdim;
	char	cdum[128];

	/* ********************************************** */
	/* Reading calibration and dimensions from header */
	/* ********************************************** */
	if(fp == NULL){
		printf("Could not open file for reading \n");
		return(-1);
		}
	else {
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		sscanf(line,"%9c %s",cdum, comment);
		fgets(line,128,fp);
		fgets(line,128,fp);
		sscanf(line,"%s %s %f %s %f %s %f",cdum, cdum, &cal[0], cdum,  &cal[1], cdum, &cal[2]);
		fgets(line,128,fp);	
		fgets(line,128,fp);
		sscanf(line,"%15c %4d",cdum, &xdim);
		fgets(line,128,fp);
      ++xdim;
		*pxdim = xdim;
	/* ************************ */
	/* Reading singles spectrum */
	/* ************************ */
		for (i = 0; i < xdim; i++){
			fscanf(fp,"%f", &ax[i]);
      }
   }
	return 0;
}


int norr2dim(FILE *fp, char *comment, int *pxdim, int *pydim, float mx[4096][512], float cal[6])
{
	char line[128];
	int i,j;
	int xdim, ydim;
	char cdum[128];

	/* ********************************************** */
	/* Reading calibration and dimensions from header */
	/* ********************************************** */
	if(fp == NULL){
		printf("Could not open file for reading \n");
		return(-1);
		}
	else {
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		fgets(line,128,fp);
		sscanf(line,"%9c %s",cdum, comment);
		fgets(line,128,fp);
		fgets(line,128,fp);
		sscanf(line,"%s %s %f %s %f %s  %f %s %f %s %f %s %f",cdum, cdum, &cal[0], cdum, &cal[1], cdum, &cal[2], cdum, &cal[3], cdum, &cal[4], cdum, &cal[5]);
		fgets(line,128,fp);	
		fgets(line,128,fp);
		sscanf(line,"%15c %4d %3s %d",cdum, &xdim, cdum, &ydim);
		fgets(line,128,fp);
	}
	++xdim;
	++ydim;
	*pxdim = xdim;
	*pydim = ydim;
	/* ************** */
	/* Reading matrix */
	/* ************** */
	for (j = 0; j < ydim; j++){
		for (i = 0; i < xdim; i++){
			fscanf(fp,"%f", &mx[i][j]);
      }
   }
	return 0;
}


int datetime(char *dattim)
{
  /* encode current date and time into character string dattim
     desired format 22-Mar-94 HH:MM:SS
     dattim must be at least 20 chars long */
  time_t   now;
  now = time((time_t *)NULL);
  strftime(dattim, 19, "%d-%b-%y %H:%M:%S", localtime(&now));
  dattim[18] = '\0';
  return 0;
}
