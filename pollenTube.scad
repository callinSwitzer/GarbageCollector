


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

union(){cylinder(60, 7,7);
    translate([10,0,2]) cube([20,10,4], center = true);
}


    cylinder(60, 7.3/2,7.3/2);
}



rotate([90, 0,0]) translate([4,0,-2.5]) linear_extrude(5) polygon(path_pts);
}}





{translate([-40,0,20])
rotate([0,90,0]) {



difference(){

cylinder(0, 18,18);
  cylinder(0, 7.3/2,7.3/2);
}
}}}



color("red")
{translate([-60,0,30])
rotate([0,90,0]) {


cylinder(60, 7.3/2,7.3/2);
}}}}
cylinder(60, 7.3/2,7.3/2);
}



//add a block in the hole if necessary
//translate([-4.8, -.75, 10]) cube([1.5,1.5,20]);
}




difference(){
{difference(){
{difference(){
translate([0,-15,0]){
    difference(){
cylinder(40, 7,7);
  cylinder(40, 7.3/2,7.3/2);}}
  
  
 translate([0,-5, 41]) rotate([90,0,0]) cylinder(40, 7,7);}}
 
 
 translate([-7,-22.5,37]) cube(15);}}
 
 
 
 // slit to see amount of pollen
 
 
 
 translate([3,-23 ,0]) cube([5, 20, 40]);}