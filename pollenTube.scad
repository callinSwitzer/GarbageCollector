


path_pts = [
    [0, 0],
    [15, 0],
    [0, 20]
];
union(){
difference(){
{difference(){
union(){
{union(){
difference(){

union(){cylinder(60, 5,5);
    translate([10,0,2]) cube([20,10,4], center = true);
}


    cylinder(60, 6.8/2,6.8/2);
}



rotate([90, 0,0]) translate([4,0,-2.5]) linear_extrude(5) polygon(path_pts);
}}





{translate([-40,0,20])
rotate([0,90,0]) {



difference(){

cylinder(0, 5,5);
  cylinder(0, 6.8/2,6.8/2);
}
}}}



color("red")
{translate([-60,0,20])
rotate([0,90,0]) {





cylinder(60, 6.8/2,6.8/2);
}}}}
cylinder(60, 6.8/2,6.8/2);
}



//add a block in the hole if necessary
//translate([-4.8, -.75, 10]) cube([1.5,1.5,20]);
}


difference(){
translate([0,-20,0]){
    difference(){
cylinder(40, 5,5);
  cylinder(40, 6.8/2,6.8/2);}}
  
  
 translate([0,-10, 40]) rotate([90,0,0]) cylinder(40, 5,5);}