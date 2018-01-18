// spherical gimbal


module ball(){
    sphere(15);
    
}



module ring(width ){
    
    translate([0, 0, -width/2]){
        difference(){
        linear_extrude(width) circle(16, center = true);
        linear_extrude(width) circle(13, center = true);}
        }
}

/*difference() {ball(); 
    ring(3.2);
    rotate([0,90,0]) ring(3.2); }
    
  */  
 module claw(){
   union(){difference(){
     ring(3);  
     translate([0, 14, 0]) cube(20, center =true);
   }
   
   translate([0, -20, 0]) cube([3, 11,3], center =true);}
 }
 
 translate([50, 0,1.5]) claw();
 
 translate([-50, 0,1.5]) claw();
 
 module halfBall(){
 difference(){
     difference() {ball(); 
    ring(3.2);
    rotate([0,90,0]) ring(3.2); }
    
     translate([-20, 0, -20]) cube(50); 
 }}
 
rotate([-90,0,0]) translate([0, 0, 30]) halfBall();
 rotate([-90,0,0]) translate([0, 0, -30]) halfBall();
 
 module arm1(){
    difference(){
cylinder(20, 7,7);

union(){cube([3.4,3.4,20], center = true);

translate([0, 0, 10]) cylinder(20, 6.8/2,6.8/2);}
} 
 }
 
 translate([10, 0,0]) arm1(); 
 
 
 module arm2(){
    difference(){
cylinder(20, 7,7);

union(){cube([3.4,3.4,20], center = true);

translate([0, 0, 10]) cylinder(20, 3,3);}
} 
 }
 
 translate([-10, 0,0]) arm2(); 