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
 cylinder(40, 10,10);
  cylinder(20, 7.5,7.5);
    cylinder(38, 6,6);
    translate([0,-2,18]) rotate([90,0,0]) cylinder(20, 4.8,4.7);
    rotate([0,0,180]) translate([0,0,18]) linear_extrude(20) polygon(path_pts);}
    