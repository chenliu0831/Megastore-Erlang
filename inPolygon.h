#ifndef _POLYGON_H
#define _POLYGON_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/*constant */
/*assume this is infinite far away*/
const int offset = 1000;
/*error bound */
const double eps = 1e-8; 

/*2D Point structure for clarity*/
typedef struct {
    	double x; 
	double y;
} point;

/*helper functions*/
/*robust for float-point error*/
bool isZero(double x) {
    return (x > 0 ? x : -x) < eps; 
}

/*cross product*/
double xmult(point A, point B, point C) {
    return (B.x-A.x)*(C.y-A.y) - (B.y-A.y)*(C.x-A.x);
}

/*
    Judge whether the given point is inside the polygon by a ray method.
	(1). Draw a ray from infinite far ending at the given point. 
	(2). Iterate through all edges and : 
		 Count the number of intersection points of the ray against the polygon edge. if odd, the point should be inside the polygon; even otherwise
	(3). Also judge first if the point is on one edge. If the ray goes through the endpoint of the edge, get another ray. 
*/
bool inPolygon(point target, point *polygon, int n) {
    int count, i = 0;
	//last point 
    polygon[n] = polygon[0];
	
	point p_far;
	
    while (i < n) {
        p_far.x = rand() + offset;  
        p_far.y = rand() + offset;   
        for (i=count=0; i<n; ++i) {
			//judge if on the edge
            if (isZero(xmult(target,polygon[i],polygon[i+1]))&&
        	    (polygon[i].x-target.x)*(polygon[i+1].x-target.x)<eps &&
		    (polygon[i].y-target.y)*(polygon[i+1].y-target.y)<eps)
                	return true;
            else if (isZero(xmult(target, p_far, polygon[i]))) break; //if ray pass the endpint, get another one
            else if (xmult(polygon[i], polygon[i+1], target)*xmult(polygon[i], p_far, polygon[i+1])>eps && 
                     xmult(target, p_far, polygon[i])*xmult(target, polygon[i+1], p_far)>eps) //intersect, add to the count 
		    ++count;
        }
    }
    return count & 1 ? true: false;
}

int  detect_point(double x, double y, double *crdsx, double *crdsy, int n)
{
	//convert to point struct for clarity
	//target point to judge
	point p_target;
	
	p_target.x = x;
	p_target.y = y;
	
	//construct struct point array for input
	point *polygon = (point *) malloc((n+1) * sizeof(point));
	
	
	for(int i=0;i<n;i++){
			polygon[i].x= crdsx[i];
			polygon[i].y = crdsy[i];
	}
	
	//judge
	if(inPolygon(p_target, polygon,n)){
		free(polygon);
		return 1;
	}else{
		free(polygon);
		return -1;
	}
	
}


#endif
