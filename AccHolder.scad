// acc holder


module acc(){
    union(){cylinder(r =7.13/2, r = 7.13/2,5); 
    
    path_pts = [
    [-7.13/2, 0],
    [7.13/2, 0],
    [2, 6.22], 
    [-2, 6.22]
];

linear_extrude(4.9) polygon(path_pts);
    
}}










module acc_space(dia){
   
    union(){cylinder(r =dia/2, r = dia/2,6.3); 
    
     path_pts2 = [
    [-8.5/2, 0],
    [8.5/2, 0],
    [2.5, 7.22], 
    [-2.5, 7.22]
];


linear_extrude(6) polygon(path_pts2);
    
}}



module accHolder(){
difference(){


translate([-6, -6, -1]) cube([12, 13, 5]);
    acc_space(8.5);}}
    
    
 
  difference(){  
   union(){ 
       accHolder();
    translate([0,5,-2])   
 difference(){   
rotate([90,0,0]) cylinder(r =4, r = 4,11);
    translate([-10, -16, 1]) cube([20, 20, 20]); }
    
    translate([0,-5,-2]) rotate([90,0,0]) cylinder(r =10, r = 10,2.5);
    }

//translate([0,10,-2])    rotate([90,0,0]) cylinder(r =0.5, r = 0.5,50);
    }




