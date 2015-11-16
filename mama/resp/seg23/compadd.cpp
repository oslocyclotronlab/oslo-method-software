/********************************************************************/
/*	Program to add and compress response functions for SuN TAS, MSU	*/
/*	Written by Magne												*/
/*	Version Sunday 24 May 2015										*/
/*																	*/
/********************************************************************/

using namespace std;

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <fstream>

int main()
{
    cout << endl;
    cout << "================================================================" << endl;
    cout << "                               COMPADD                          "<< endl;
    cout << "Program to add and compress response functions for SuN TAS, MSU " << endl;
    cout << "You should have an empty folder called s for storing the results" << endl;
    cout << "                          Magne Guttormsen                      " << endl;
    cout << "                         Version 24 May 2015	                 " << endl;
    cout << "================================================================" << endl;
    cout << endl;
    
    // File pointer
    FILE *infile1, *infile2, *outfile;
    
    // Declaration of variables
    int spec_no;
    char filename1[50], filename2[50], filenameout[50];
    int energy_keV = 0;
    char line[30];
    int max_ch   = 12000;
	double Counts1[12000];
    double Counts2[12000];
    double Countsm[12000];
    double Countsc[12000];
    int comp = 4;
    int step;
	int i, j, k, l, m, x;
	double y;
    float a0 = 0., a1 = 1.;    //input spectra has shift 0 keV and gain 1 keV/ch
    
	// Opening files, read files, fill vectors and write out
    spec_no  = 25;
    for(i=0; i<spec_no; i++){
        // string of input file name, scissors
        if(energy_keV >=    0 && energy_keV <  3000)step =  200;
        if(energy_keV >= 3000 && energy_keV <  6000)step =  500;
        if(energy_keV >= 6000 && energy_keV < 15000)step = 1000;
        energy_keV = energy_keV + step;
        sprintf(filename1,  "resp_func/%2.2d/seg2_%2.2d.asc" ,energy_keV,energy_keV);
        sprintf(filename2,  "resp_func/%2.2d/seg3_%2.2d.asc" ,energy_keV,energy_keV);
        sprintf(filenameout,"s/s%2.2d",energy_keV);
        printf("%2.2d Seg file name is %s \n",i+1,filename1);
        printf("%2.2d Seg file name is %s \n",i+1,filename2);
        printf("%2.2d Seg file name is %s \n",i+1,filenameout);

        // open seg2 file and read spectrum
        infile1 = fopen(filename1, "r");
        if(infile1==NULL){
            printf(" Could not open the input file!\n");
            exit(1);
        }
        for(j=0; j< max_ch; j++){
            fscanf(infile1,"%d %lf",&x,&y);
            Counts1[j] = y;
        }
        fclose(infile1);
        // open seg3 file and read spectrum
        infile2 = fopen(filename2, "r");
        if(infile2==NULL){
            printf(" Could not open the input file!\n");
            exit(1);
        }
        for(j=0; j< max_ch; j++){
            fscanf(infile2,"%d %lf",&x,&y);
            Counts2[j] = y;
        }
        fclose(infile2);
        
        //////////////////////
        // Fasten seatbelts //
        //////////////////////
        
        // Taking average of seg2 and seg2
        for(j=0; j < max_ch; j++){
            Countsm[j] = (Counts1[j] + Counts2[j])/2.;
        }
        // Compressing comp times and writing out
        outfile = fopen(filenameout, "w");
        if(outfile == NULL){
            printf("Could not open file resp_func/xxx/syyy \n");
            exit(0);
        }
        l = 0;
        for(j = 0; j < max_ch; j = j + comp){
            Countsc[l] = 0;
            for (k = 0; k < comp; k++){
                Countsc[l] = Countsc[l] + Countsm[j+k];
            }
            /* ***************************************** */
            /* Saving the addded and compressed spectrum */
            /* ***************************************** */
            fprintf(outfile, " %lf  \n", Countsc[l]);
            l++;
        }
            fclose(outfile);
    }
    // Spectra will then have comp*(keV/ch), since original spectra have 1 keV/ch
    // Write out new a0 and a1 calibration coefficients
    printf(" New calibration coefficients are a0 = %4.1f keV, a1 = %4.1f keV/ch \n", a0 + a1*(comp-1.)/2. ,a1*float(comp));
    
} // END of program
