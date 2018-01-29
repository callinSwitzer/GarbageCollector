/*difference(){
cylinder(16, 7,7);
  cylinder(16, 7.3/2,7.3/2);}
*/ 

path_pts = [
    [-6, 0],
    [6, 0],
    [0, 40]
];
  
  
difference(){
cylinder(20, 14,14);
  cylinder(4, 8,8);
    cylinder(18, 6,6);
    //translate([0,-2,4]) rotate([90,0,0]) cylinder(20, 6,2.5);
    rotate([0,0,180]) translate([0,0,-2]) linear_extrude(20) polygon(path_pts);}
    
    
    
  




