#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "inPolygon.h"


int main() {

	//input file handle
	
	FILE *fin;

	fin = fopen("input.dat","r");
	//num of points
	int N;
			
	scanf(fin,"%d",&N);
	
	double *coord_x = (double*) malloc(N*sizeof(double));
	double *coord_y = (double*) malloc(N*sizeof(double));
	
    	for (int i=0; i<N; ++i) 
		fscanf (fin,"%lf%lf", &coord_x[i], &coord_y[i]);
  
   	fclose(fin); 
	
	//simple test
    	double point_in_x = 0.00;
	double point_in_y = 1.5;
	
	double point_out_x = -1.5;
	double point_out_y = -1;
       
	//test in polygon point
	if (detect_point(point_in_x,point_in_y,coord_x,coord_y,N) >0) 
		printf ("Point (%lf,%lf) is inside the polygon\n", point_in_x,point_in_y);
	else 
		printf ("Point (%lf,%lf) is outside the polygon\n", point_in_x,point_in_y);
    
	//test outside polygon point
	if (detect_point(point_out_x,point_out_y,coord_x,coord_y,N) >0) 
		printf ("Point (%lf,%lf) is inside the polygon\n", point_out_x,point_out_y);
	else 
		printf ("Point (%lf,%lf) is outside the polygon\n", point_out_x,point_out_y);
	
	//free memory
	free(coord_x);
	free(coord_y);
        return 0;
}
