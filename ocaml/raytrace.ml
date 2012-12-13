
(** VECTORS ***************************************************************)
type vector = float * float * float;;
let zerov = (0., 0., 0.)

let (^+) (x1,y1,z1) (x2,y2,z2) = (x1+.x2, y1+.y2, z1+.z2);; 
let (^-) (x1,y1,z1) (x2,y2,z2) = (x1-.x2, y1-.y2, z1-.z2);;
let (^*) (x,y,z) d = (x*.d, y*.d, z*.d);; 
let length (x,y,z) = sqrt ((x *. x) +. (y *. y) +. (z *. z));;
let string_of_vector (x,y,z) = Printf.sprintf "<%f,%f,%f>" x y z;;
let normalize v = v ^* (1. /. (length v))
let dot (x1,y1,z1) (x2,y2,z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2);;
let cross (x1,y1,z1) (x2,y2,z2) =
  let xn = y1 *. z2 -. z1 *. y2
  and yn = z1 *. x2 -. x1 *. z2
  and zn = x1 *. y2 -. y1 *. x2 in
    (xn,yn,zn)
;; 
let distance v1 v2 = length (v1 ^- v2) ;; 
let invert (x,y,z) = (-.x,-.y,-.z);;

let string_of_vector (x,y,z) = Printf.sprintf "<%f,%f,%f>" x y z;;

(** RAYS *****************************************************************)
(* Origin, direction *)
type ray = {rdir : vector; rorigin : vector};;


(** MATERIALS ************************************************************)
(* r, g, b *)
type color = float * float * float;;
(* Color, reflection, diffuse *)
type material = {mcolor : color; mspecular : float; mdiffuse : float };;

let clampUnit w = max 0.0 (min 1.0 w)
let colorClamp (r, g, b) = (clampUnit r, clampUnit g, clampUnit b) 
let black = (0.0, 0.0, 0.0)

(** PRIMITIVES ************************************************************)
type shape =
    (* radius *)
    Sphere of float
      (* Normal, distance (?) *)
  | Plane of vector * float
type prim = {
  pmat : material;
  ppos : vector; (* XXX: Relevant? *)
  pname : string;
  pshape : shape;
}
type light = {
  lpos : vector;
  lcolor : color;
}

type scene = {
  prims : prim list;
  lights : light list;
  ambient : color;
}

type intersection = {dist : float; norm : vector; prim : prim};;
 
(* Returns distance and normal of the surface whree it hit *)
let intersectSphere ray position radius =
  let v = position ^- ray.rorigin in
  let b = (dot v ray.rdir) in
  let d2 = (b *. b) -. (dot v v) +. (radius *. radius) in
  let dist = 
    if d2 < 0. then infinity
    else
      let d = sqrt d2 in
      let t1 = b -. d
      and t2 = b +. d in
        if t2 > 0. then
          if t1 > 0. then t1
          else t2
        else
          infinity 
  in
  let hitpoint = ((normalize ray.rdir) ^* dist) ^+ ray.rorigin in
  let norm = normalize (hitpoint ^- position) in
    (dist, norm) 
;;

(* Returns the distance at which a ray intersects with the given prim, or
 * infinity if it does not.
 *)
let intersectPrim ray prim =
  match prim.pshape with
      Sphere( r ) -> let dist, norm = intersectSphere ray prim.ppos r in
	if dist < infinity then Some {dist = dist; norm = norm; prim = prim}
	else None
    | Plane( norm, d ) -> None (* intersectPlane ray prim.pos dist norm d *)
;;

let traceScene ray scene =
  List.fold_left
    (fun oldHit prim ->
       let newHit = intersectPrim ray prim in
         match (oldHit, newHit) with
             (_, None) -> oldHit
           | (Some( oldt ), Some( newt )) -> 
               if oldt.dist > newt.dist then newHit else oldHit
           | (None, _) -> newHit)
    None scene.prims
;;

let lightColor (lr,lg,lb) (cr,cg,cb) = (lr *. cr, lg *. cg, lb *. cb);;

(* We shall do Blinn model specular reflection, a la
http://www.siggraph.org/education/materials/HyperGraph/illumin/specular_highlights/blinn_model_for_specular_reflect_1.htm

   S = DGF/(N.E)
   S = specularity
   D = distribution function of facets
   G = how much facets shade each other
   F = fresnel reflection
   N = normal of surface
   E = angle of incidence of the viewer.
   
   D = (c^2/((cos(alpha)^2 * c^2 - 1) +1)^2)
   where c = 0 for very shiny surfaces and 1 for very dull surfaces
   and alpha = acos(N.H)
   
   G = min( Ga, Gb, Gc ) where
   Ga = 1.0
   Gb = 2(N.H)(N.E)/(E.H)
   Gc = 2(N.H)(N.L)/(E.H)
   where H = (L + E) / 2
   and L = incidence angle of light.
   
   F = 0.5 * ((g-c)^2 / (g+c)^2) * (1 + ((c(g+c)-1)^2)/((c(g-c)+1)^2))
   n = index of refraction
   c = E.H
   g = sqrt( n^2 + c^2 - 1 )
   
   
   Or we can just use Phong shading, which seems to be this:
   shading = ambientcolor * ambientconstant + diffusecolor * dot(L, N) + specularcolor * pow(dot(R, V), falloff)
   Where N is the surface normal, L is the vector from the point to the light source and V is the vector to the viewer.  R is the reflection of L around N.
   Falloff is some term to make the shininess falloff more intense.
	 
   We can get R = L - 2(L.N)*N 
*)
let traceLight ray hit scene =
  let rayToLight light =
    let hitpoint = ((normalize ray.rdir) ^* hit.dist) ^+ ray.rorigin in
    let lightdir = normalize (light.lpos ^- hitpoint) in
    let lightray = {rorigin = hitpoint; rdir = normalize lightdir} in
    let mat = hit.prim.pmat in
    let n = normalize hit.norm
    and l = normalize lightray.rdir
    and v = normalize ray.rdir in
    let diffuse = (mat.mcolor ^* mat.mdiffuse) ^* (dot l n)
    and ambient = black (* lightColor mat.mcolor scene.ambient *) in 
      if (dot l n) > 0. then
	let p = List.filter ((<>) hit.prim) scene.prims in
	let newScene = {scene with prims=p} in
	  match traceScene lightray newScene with
	      Some( p ) -> colorClamp ambient (* Shadow! *)
	    | None ->
		let r = normalize (l ^- (n ^* (2. *. (dot l n)))) in
		  (* XXX: The exponent has to be odd.  It SHOULDN'T, 
		     but it does. *)
		let specular = (mat.mcolor ^* mat.mspecular) ^*
		  ((dot r v) ** 3.0) in
		  colorClamp (ambient ^+ diffuse ^+ specular)
      else
	ambient
  in
    List.fold_left 
      (fun color light -> colorClamp (color ^+ (rayToLight light)))
      black scene.lights
;;

(* Takes a given ray, returns a color. *)
let raytrace ray scene =
  match traceScene ray scene with
      None -> (0., 0., 0.)
    | Some( hit ) -> traceLight ray hit scene (* hit.prim.mat.mcolor *)
;;




(** ENGINE & METADATA *******************************************************)
type pixel = int * int * color
(* Field of view is in radians *)
type camera = {cpos : vector; ctarget : vector; cfov : float}

type raytracer = {
  (* Size of the output picture *)
  size : int;
  camera : camera;
  output : pixel list;
  tracedepth : float;
}

(* This assumes the screen rectangle is -1 to +1 the given plane point in the
 * x and y directions,
 * and the normal is in the +z direction.
 *)
let cameraRay x y planepoint focuspoint splits =
  let px,py,pz = planepoint in
  let divisions = 1. /. (float_of_int (splits / 2)) in
  let planex = (float_of_int (x - (splits / 2))) *. divisions
  and planey = (float_of_int (y - (splits / 2))) *. divisions in
  let raydir = normalize (focuspoint ^- (planex, planey, pz)) in
    {rdir = raydir; rorigin = focuspoint}
;;
   

let r = {
  size = 800;

  output = [];
  tracedepth = 0.0;
  camera = {cpos = zerov; ctarget = zerov; cfov = 0.30};
};;

let color_of_float f =
  max 0 (min 255 (truncate (f *. 255.)))
;;

let render rt scene =
  let chan = open_out_bin "fop.rgb" in
    for y = 0 to rt.size - 1 do
      for x = 0 to rt.size - 1 do
(* Ortho projection *)
(*	let fx = ((float_of_int (x - 400)) /. 5.)
	and fy = ((float_of_int (y - 300)) /. 5.) in
	let ray = {rorigin = (fx, fy, -5.0); rdir = (0., 0., 1.0)} in 
*)
	let ray = cameraRay x y zerov (0., 0., 1.) rt.size in 
        let (r,g,b) = (raytrace ray scene) in
        let rb = color_of_float r
        and gb = color_of_float g
        and bb = color_of_float b in
          output_byte chan rb;
          output_byte chan gb;
          output_byte chan bb;
      done;
    done;
    close_out chan;
;;


let testScene = {
  lights = [{lpos = (5., 1., 1.); lcolor = (0.0, 0.0, 1.)};
(*	   {lpos = (-10., 1., 1.); lcolor = (1.0, 0.0, 0.0)}; *)
	   ];

  prims = [
  (*
    {pmat = ((0.4, 0.3, 0.3), 0., 1.);
    pshape = Plane( (0., 1., 0.), 4.4);
    pname = "plane";
    ppos = (0., 0., 0.)
    };
  *)
  {pmat = {mcolor = (1.0, 0.7, 0.7); mspecular = 0.6; mdiffuse = 0.4;};
   pshape = Sphere( 3. );
   pname = "big sphere";
   ppos = (1., 1., 10.)
  };
  {pmat = {mcolor = (0.0, 0.3, 0.0); mspecular = 0.6; mdiffuse = 0.1;};
   pshape = Sphere( 0.2 );
   pname = "shadowing sphere";
   ppos = (3.0, 1., 7.)
  };
  {pmat = {mcolor = (0.6, 0.6, 1.0); mspecular = 0.9; mdiffuse = 0.4;};
   pshape = Sphere( 2. );
   pname = "small sphere";
   ppos = (-10.0, -10.0, 50.);
  };
  ];
  ambient = (0.1, 0.1, 0.1);
};;

render r testScene;

(* View output with: convert -depth 8 -size 800x800 rgb:fop.rgb x: *)
(* Convert convert -depth 8 -size 800x800 rgb:fop.rgb png:whatever.png *)
