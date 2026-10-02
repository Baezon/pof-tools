# Reverse engineering BSPGEN's moment of inertia

BSPGEN was Volition's model converter. It wrote the mass, center of mass and moment of inertia into the header of every retail POF. Its source and its binary are lost. This folder holds what has been worked out about how it arrived at those numbers, and the scripts that worked it out.

The aim is for pof-tools to be able to reproduce the retail numbers from a model's geometry. How BSPGEN went about it is now known. It laid a lattice of samples over the hull, judged each sample by casting a ray, and added up those inside in single precision. Worked from nothing but the POF, that comes within 0.2% of the retail tensor on the median model, where the formulas with exact integrals come within 1%.

It is not yet a match to the digit. Two things stand in the way: which floats the verts were when the rays were cast, and a handful of samples that BSPGEN judged otherwise than any test tried here, on even the simplest hulls.

Status as of 2026-09-29.

## Summary

| Question | Answer | Confidence |
|---|---|---|
| Which geometry was weighed? | The detail 0 submodel alone. No children, no turrets, no other detail levels, no debris | High |
| What scale was the source brought in at? | 0.0254, inches to metres. The log prints it as "0.025" | High |
| What is the mass? | The volume of that submodel, turned into `4.65 * volume^0.6667` from POF version 2009 on | High |
| What is the center of mass? | The centroid of that volume | High |
| What is the moment of inertia? | `I_central + m * (\|c\|^2 - 1) * identity` | High |
| Did FreeSpace 2's BSPGEN work the same way? | Yes | High |
| How was it integrated? | By counting the samples of a lattice that lie inside the hull, with every sum kept in a float | High |
| What was the lattice? | Cells fitted to the hull's bounding box, nearly cubes, some 5000 of them across the box as seen along z, with a sample at the middle of each | High |
| How was a sample judged? | By a ray along z. It is inside if an odd count of the ray's crossings lie below it | High for the rule. The last details are not settled |
| How did it cast a ray? | By `fvi_point_face` as Graphics Gems has it, against the polygons of the POF, in metres | High |
| What are the cross sections in the header? | The reach of a grid of rays, 25 by 25, cast along y through the hull | High |
| How were open meshes handled? | By the same count of crossings | Medium |

## What there is to work from

| Material | Where | What it gives |
|---|---|---|
| Volition's source release of 2002-04-24 | not in this repository | The reader for the header, and a dated changelog. BSPGEN itself isn't in it |
| FreeSpace 2 retail POFs, 176 of them | the retail `sparky` archives | The numbers to be matched, and BSPGEN's log |
| FreeSpace 1 POFs with their `.p3d` sources, 94 pairs | supplied by Goober5000 | The geometry BSPGEN started from, beside what it made of it |

None of the models are in this repository. The scripts take the folders that hold them as their arguments.

## From the source release

The release reads the tensor and nothing more. It is read in `code/Model/ModelRead.cpp` and copied as it stands into the physics, in `code/Ship/Ship.cpp`, as `I_body_inv`. So what the header stores is the inverse of the inertia tensor.

The changelog at the top of `ModelRead.cpp` dates three steps:

| Date | Author | Entry |
|---|---|---|
| 9/4/97 | Andsager | "implement physics using moment of inertia and mass (from BSPgen)" |
| 1/28/98 | Mike | "Convert volume to surface area at runtime" |
| 1/29/98 | Andsager | "Changed mass and moment of inertia based area vs. volume" |

The reader shows what the last two mean. Before POF version 2009 the mass in the file is a volume. The reader turns it into an "area mass" and scales the tensor to match:

```
area_mass = 4.65 * vol_mass^0.6667
tensor   *= vol_mass / area_mass
```

From version 2009 on, BSPGEN did that itself. Every FreeSpace 2 retail model is version 2116 or 2117. The FreeSpace 1 models run from 1901 to 2014, and 45 of the 94 are below 2009.

The power is 0.6667 and not two thirds. Twelve FreeSpace 2 models had their mass converted from a FreeSpace 1 volume, and they agree with that power to the last digit. It matters when turning a mass back into a volume: the two powers differ by 0.07% on a large model.

## From BSPGEN's log

Every POF carries a `PINF` chunk holding the log of the run that made it. For `cargo02.pof`:

```
Started with 388 vertices and 730 faces.
Ended with 382 vertices and 580 faces.
Texture splitting created 0 vertices and 0 faces.
Command line: BSPGEN cargo02.p3d
Scale factor:   0.025
```

- **BSPGEN changed the faces and kept the vertices.** It merged or removed faces, heavily in places, which is why so many retail meshes are open as stored. The vertices of a hull in the POF are those of the source to within a hundred thousandth of a metre.
- **Its input was a `.p3d` file.**
- **It took two flags,** `-b` and `-c` with a number, mostly 25. The number is the count of cross sections, which are taken up below. What `-b` does isn't known. Neither one separates the models that fit from those that don't.

`bspgen_logs.py` prints the log of every POF in a folder.

## The P3D format

A `.p3d` is a 3D Studio `.3ds` file as 3DS MAX exported it, with three differences:

| | `.3ds` | `.p3d` |
|---|---|---|
| Root chunk | `0x4D4D` | `0xBEEF` |
| A face | 8 bytes: three vertex indices and flags | 32 bytes: the same, then six floats of texture coordinates, a pair to a corner |
| An object | | has a 9 byte chunk tagged `0xDEAD`, of unknown meaning |

Vertices are in scene coordinates and scene units. The keyframer block gives each object's name, parent, pivot and position. `p3d.py` reads all of it, and prints the chunk tree of a file when run on one.

## Findings from the FreeSpace 1 pairs

### The scale is 0.0254

The bounding box of each hull in the POF is 1.0160 times what a scale of 0.025 gives, on every axis of every model. That is 0.0254 over 0.025. The log prints the scale to three places.

Getting this wrong puts every volume out by 4.9%, which looks very much like a fault in the method.

### The axes

POF coordinates are `(-X, Z, -Y)` of the scene. The order follows from the sizes of the bounding boxes. The signs follow from the stored center of mass: of the models lopsided enough to tell, 54 choose these signs. The rest of the counts in `check_centroid.py` are models that are the same on both sides, which can't tell the two signs of x apart.

This is a reflection, so the winding of every face is reversed by it.

### Only the hull is weighed

`check_volume.py` sets the volume that the stored mass stands for against the exact volume of the source mesh. Over the 48 ship-like models that have anything below their hull:

| Stored volume, over the volume of | 10th percentile | Median | 90th percentile | Within 3% of 1 |
|---|---|---|---|---|
| the detail 0 submodel alone | 0.996 | 1.009 | 1.027 | 40 of 48 |
| detail 0 and everything below it | 0.557 | 0.880 | 0.992 | 10 of 48 |

### The center of mass is the hull's centroid

Over the 47 closed hulls, the stored center of mass lies a median of 0.27% of the model's radius from the exact centroid of the hull, and within 1% on 40 of them.

### The tensor

`check_tensor.py` works out each candidate exactly from the hull, inverts it, and compares all nine entries with the stored tensor. Over the 47 closed hulls:

| Candidate | Median error | Within 1% | Within 3% | Within 10% |
|---|---|---|---|---|
| About the model origin | 7.4% | 4 | 15 | 25 |
| About the center of mass | 10.0% | 5 | 15 | 23 |
| About the center of mass, with `m * |c|^2` on the diagonal | 2.9% | 13 | 25 | 39 |
| About the center of mass, with `m * (|c|^2 - 1)` on the diagonal | 1.0% | 22 | 39 | 46 |

The last is

```
I = I_central + m * (|c|^2 - 1) * identity
```

with `c` the center of mass as measured from the model origin, in metres.

It has two departures from the parallel axis theorem, which would give `I_central + m * (|c|^2 * identity - c * cT)` for the tensor about the origin:

- **The term `c * cT` is left out.** What is added is the same on every axis and has nothing off the diagonal.
- **The mass times the identity is taken off.** The 1 is a bare number, which is to say a square metre.

Neither was guessed at. `solve_extra_term.py` takes the stored tensor, removes the exact central tensor, divides by the mass, and prints what is left. On a large model that is `|c|^2` on each axis. On a small one it falls short of `|c|^2` by 1:

| Model | Radius | What is left, less `|c|^2`, on x, y and z |
|---|---|---|
| `navbuoy` | 5.5 | -0.979, -0.981, -0.973 |
| `gunplatform01` | 6.1 | -0.965, -0.958, -0.989 |
| `unknownship` | 6.3 | -0.968, -1.064, -1.002 |
| `fighter01` | 9.8 | -0.967, -0.966, -1.083 |
| `fighter07` | 11.8 | -0.977, -0.921, -0.978 |
| `asteroid03` | 15.0 | -1.038, -1.024, -1.028 |
| `support02` | 16.4 | -0.976, -0.942, -1.022 |
| `freighter01` | 33.2 | -1.086, -1.063, -1.042 |

On a large model the 1 is lost in the noise, a square metre being nothing beside a radius of gyration of fifty.

The three asteroids of FreeSpace 1 show it best. They are one mesh at three sizes, the same to seven digits. With the 1 left out, the largest fits to 0.03% and the smallest is 2.9% out. With it, all three fit to within 0.1%.

How it would have been written isn't known. It reads like a parallel axis step with the identity where the outer product of `c` should be.

### What the formula explains

Two puzzles of the first pass are settled by it.

- **Some models seemed to follow the plain central tensor.** They were the small ones whose `|c|^2` happens to be near 1, so that the term all but vanishes: `fighter01` at 1.03, `fighter07` at 1.07, `escapepod02` at 0.87.
- **`gunplatform01` was 23% out though its volume and center of mass matched.** Its center of mass is at the origin, so the term is the mass times the identity taken off, and nothing else. It now fits to 1.0%.

### Weapons fit nothing

The tensors of the missiles and lasers are 10 to 50 times what their geometry gives, and wrong in their proportions as well. The engine never used them, so nobody would have noticed. They are left out of every count here, and should be left out of any fit.

## FreeSpace 2 against FreeSpace 1

75 models are in both games. `check_fs2.py` sorts them by what became of their numbers:

| What became of the mass properties | Hull unchanged | Hull changed |
|---|---|---|
| Carried over as they were | 16 | |
| Converted from a volume by the loader's formula | 12 | |
| Worked out afresh | 41 | 5 |

The 41 worked out afresh from an unchanged hull are two runs of BSPGEN on one mesh. Set against the FreeSpace 1 source, the FreeSpace 2 tensors fit the formula as the FreeSpace 1 tensors do:

| Tensor of | Models | Median error | Within 3% | Within 10% |
|---|---|---|---|---|
| FreeSpace 2 | 37 | 1.95% | 24 | 35 |
| FreeSpace 1, the same models | 37 | 2.09% | 23 | 35 |

So BSPGEN did the same thing in both games. These counts take in the open hulls, which is why they are worse than the 1.0% above.

## The cross sections, and how BSPGEN cast a ray

From POF version 2014 on, the header follows the tensor with a table of cross sections, each a depth along z and a radius. Eleven of the 94 FreeSpace 1 models have one, and 64 of the 176 of FreeSpace 2. Each has 25 entries, and each was built with `-c25`.

`check_cross_sections.py` rebuilds the table from the hull:

1. The hull's bounding box is cut into 25 slabs along z, and into 25 strips along x.
2. At the middle of each slab, through the middle of each strip, a ray is cast along y.
3. The radius of a slab is how far the farthest point that its rays cross the hull at lies from the z axis.

The axis is that of the scene, and so are the depths. BSPGEN moved the origin of the model after it had measured.

| The rays cast by | FreeSpace 1: entries rebuilt to within a thousandth, and a hundred thousandth | Both games: the same |
|---|---|---|
| an exact test, against the triangles of the source | 197 and 191 of 200 | 242 and 233 of 250 |
| BSPGEN's test, against the polygons of the POF | 200 and 200 of 200 | 250 and 250 of 250 |

The 200 are 8 of the 11 models. Two more are copies of `capital01` under other names. The last is `cruiser03`, whose depths start a metre before its hull does: its source is not the scene that its POF was made from. The FreeSpace 2 tables show the same of `freighter02`, `freighter04` and `freighter07`.

### The test

Where the two tests differ, BSPGEN has a ray hitting a polygon that it passes outside of, or missing one that it passes through. Rounding decides which, so the entries that an exact test misses show what BSPGEN's sums were, down to the order they were done in. `fvi.py` is the test that rebuilds every entry.

It is `fvi_ray_plane` followed by `fvi_point_face`, both of `code/Math/Fvi.cpp` in the FreeSpace 2 source, with one difference. Where the source asks whether the first edge of a triangle runs within 0.0001 of straight along an axis, BSPGEN asked whether it runs exactly straight, as the routine in Graphics Gems that both come from does.

- **The coordinates are in metres,** each the float of its inches times the float of 0.0254, with the axes of the POF.
- **The polygons are those of the POF,** which BSPGEN made by merging the triangles of the source, in the POF's order of verts and with the normals stored for them.
- **A polygon is tested as a fan of triangles from its first vert,** and its plane is taken through that vert. The test stops at the first triangle that holds the point.
- **The edges of a triangle count as inside it.** A ray through an edge that two polygons share crosses both.
- **The arithmetic is that of a compiler of 1997 on the x87.** Sums and products are carried at the width of a double, and cut to single precision on being stored in a variable.

### The fault in it

The test finds where a point lies in a triangle by two numbers, `beta` and `alpha`, and works out the second as `(u0 - beta * u2) / u1`, where `u1` is how far the first edge of the triangle runs along one axis of the projection. Models are full of edges that were drawn straight along an axis and came out one step of a float off it. For such an edge `u1` is a millionth of a metre or less, what it divides is the difference of two nearly equal numbers, and the rounding of `beta` decides the answer.

So a ray can be taken to hit a triangle that it passes metres outside of, or to miss one that it passes through. Volition's margin of 0.0001 is there to stop this. BSPGEN's test had no margin.

## How the hull was weighed

### The lattice

```
side  = sqrt(size_x * size_y / 5000)
count = ceil(size / side)          on each axis
step  = size / count               on each axis
```

The sizes are those of the box about the hull's own verts. With the box of the whole model, which is the one in the header, no count of samples gives the stored mass. The cells are nearly cubes, a little over 5000 of them cover the box as seen along z, and a sample is taken at the middle of each.

| Model | Cells |
|---|---|
| the three asteroids | 62 by 82 by 64 |
| `cargo04` | 79 by 64 by 130 |
| `fighter01` | 136 by 37 by 95 |

### The volume is a count, kept in a float

The volume is a float that had the volume of a cell, `step_x * step_y * step_z`, added to it once for each sample inside. Adding the same small number to a float a hundred thousand times rounds the same way every time, so the sum drifts from the count times the cell by up to 0.3%.

The sum is one running total. With a subtotal to each row or layer it comes out thousands of steps of a float from the stored volume.

That drift is what makes the scheme certain. With the right cell, the stored mass can be had to the bit from some count of samples. With a cell of any other size it can't, but for a chance of a few in a hundred.

| | Models | Have a count that gives the stored mass to the bit |
|---|---|---|
| FreeSpace 1, closed hulls | 47 | 47 |
| FreeSpace 2, all but the weapons | 158 | 156 |

From version 2009 on the mass is `(float)(4.65f * pow(volume, 0.6667f))`, both constants being floats. That gives the mass to the bit on all 21 closed hulls of those versions in FreeSpace 1. With either constant a double, it does so on 15 at the most.

The drift is in the volume alone, since the moments are summed apart from it. The three asteroids of FreeSpace 1, one mesh at three sizes, show it. All three have 115464 samples inside. Their volumes differ by 0.1% of one another, because the bits of the cell differ with the size, and their moments don't.

### The sums

Beside the volume, nine sums are kept, each in a float: of `x`, `y` and `z`, of `y*y + z*z`, `x*x + z*z` and `x*x + y*y`, and of `x*y`, `y*z` and `z*x`. The places are in metres from the origin of the model. Each sum is multiplied by the cell at the end.

The tensor made from them is the one about the origin, less the volume times `identity - c * cT`. That is the formula found above, reached from the other side.

The samples are taken with x outermost and rising. The three asteroids show it: of the 48 orders and directions the loops could have, the 8 with x outermost and rising bring the sums of the three sizes within 0.05 to 0.11 of a cell of one another, and every other leaves 0.85 or more. Of the two orders for the rest, z before y fits a little better. The order matters only to the rounding, in the seventh digit.

Sums about any other point than the origin of the model, a corner of the box or the origin of the scene among them, leave the tensor 3 to 90 cells out.

### How a sample is judged

A ray is cast along z through each column of samples, and a sample is inside if an odd count of the ray's crossings lie below it. A ray cast down z from each sample in turn comes to the same.

`check_lattice.py` lays the lattice over each closed hull of FreeSpace 1 and judges the samples three ways. Over the 47:

| Samples judged | Samples off, median | Tensor off, median | Within 0.01% | Within 0.1% | Within 1% |
|---|---|---|---|---|---|
| No lattice: exact integrals in the formula | | 1.02% | 0 | 3 | 22 |
| Exactly | 575 | 0.80% | 4 | 10 | 26 |
| By BSPGEN's test, verts measured from the origin of the scene | 149 | 0.11% | 4 | 23 | 41 |
| By BSPGEN's test, verts measured from the origin of the model | 67 | 0.11% | 4 | 21 | 40 |

| Model | Samples | Off, judged exactly | Off, judged by BSPGEN's test |
|---|---|---|---|
| `drone02` | 280505 | -7175 | -129 |
| `fighter01` | 77407 | -835 | -11 |
| `navbuoy` | 24932 | +137 | -26 |
| `bomber05` | 217602 | -270 | +21 |

The ray runs along z, and the crossings counted are those below. With the crossings above, or with rays along x or y, the median is 527 to 824 samples off, and the crossings below are the best of the six on 29 hulls of 45.

### Why the errors look as they do

A ray that misses the polygon at the bottom of a column turns the whole column over. The samples inside the hull are lost, and those above it are gained, up to the top of the box. A ray that hits a polygon it passes outside of does the same from wherever it crosses that polygon's plane.

- **Ships are out by 2% and the simple hulls by 0.1%.** A ship has hundreds of polygons, a few of them with an edge a step off straight. `asteroid02` and `cargo04` have none.
- **The errors lean high.** Most hulls are thin beside their boxes, so a column turned over gains more than it loses.
- **The error falls the same way each run.** It comes of the rounding, which is the same each time.
- **The errors of a hull that is the same on both sides are lopsided.** The fan of a polygon starts from a vert on one side, and its mirror image starts from another.

### From the POF alone

Everything the scheme needs is in the POF: the verts of the hull, its polygons as BSPGEN merged them, and their normals. `check_pof_alone.py` works from those and nothing else. Over the models of FreeSpace 2, less the weapons:

| Hulls | Models | Mass off, median | Center of mass off, over the longest side | Tensor off, median | Tensor within 0.1% | Within 1% | Within 3% |
|---|---|---|---|---|---|---|---|
| Whole | 107 | 0.05% | 0.02% | 0.18% | 36 | 90 | 102 |
| With edges left open | 51 | 0.05% | 0.03% | 0.28% | 13 | 45 | 48 |

A hull with holes is weighed by the same count of crossings, and comes out nearly as well.

### What is still out

**Which floats the verts were.** The verts of the POF are those of the scene with the origin of the model taken from each, and rounded again. Two verts a step apart in the one can be the same in the other, and a fault comes or goes with that. Neither fits throughout:

| Model | Samples off, verts from the scene | Verts from the model |
|---|---|---|
| `fighter10` | +2756 | -168 |
| `fighter06` | +780 | +10 |
| `freighter01` | +784 | -32 |
| `gunplatform01` | -268 | -2 |
| `asteroid03-01` | +105 | +4 |
| `drone02` | -129 | -4121 |
| `fighter12` | -2 | -71 |
| `cruiser02` | +202 | +351 |

Verts measured from a corner of the box, from its middle or from the center of mass do no better, nor does the one frame on some axes and the other on the rest. Sixteen ways of rounding the fan test were tried in both frames, and the one that rebuilds the cross sections is the best of them.

**A few samples on the cleanest hulls.** These have no polygon that the test takes wrongly, in either frame, and still BSPGEN differs:

| Model | Samples BSPGEN had | The lattice has |
|---|---|---|
| the three asteroids | 115464 | 7 more |
| `unknownship` | 51739 | 11 more |
| `cargo06` | 123646 | 3 more |
| `cargo04` | 605338 | 11 fewer |
| `cargo03` | 654796 | 149 more |

The 149 of `cargo03` are one whole column along z, at or beside the 62nd cell along x and the 4th along y. The others are not one column or two. The asteroids are out by the same 7 at all three sizes, so whatever does it does not come of rounding. Tried and ruled out: samples elsewhere in their cells, a shift or stretch of the lattice, samples placed by adding the step over and over, the samples nearest the surface, filling from the first crossing to the last, and rays cast from each sample in some other direction or toward some one point, which on these hulls give the samples that rays along z do.

What is known of it:

- **It is not the ray test.** These hulls give the same samples by the triangles of the source, by the polygons of the POF tested exactly, and by `fvi.py`. Where the plane of a polygon is taken through makes no difference.
- **It has a handedness.** `cargo04` is the same on both sides of x, and `cargo06` and `unknownship` of z, and so are their lattices. The stored center of mass lies off the plane of symmetry all the same, by 0.29, 0.30 and 0.20 of a millimetre, and to the positive side each time.
- **It is more than the count says.** The products of `cargo04` ask for some 45 samples to have changed, gains and losses together, where the count is out by 11.
- **The three asteroids are not quite alike.** Their first moments differ by a tenth of what is left, which is more than the rounding of the sums can do.

Also ruled out: the lattice turned or shifted as a whole, a sample tested a little to one side of where it is summed, one to three crossings miscounted or made up along any axis, the spheres and boxes of the tree that the polygons hang in, and a nudge by `rand()`.

Their tensors are within 0.002% to 0.05% all the same. `cargo03` with its one column taken out by hand has its mass to the bit, its center of mass to 2 parts in a million and its tensor to 7.

**What changed between the games.** The hulls worked out afresh for FreeSpace 2 have numbers 0.2% from those of FreeSpace 1. On the four looked at, the verts are the same to the bit and in the same order, and the normals differ: by one step of a float, in two polygons of five. A ray along z is all but blind to such a change, since where it crosses a polygon that faces along z does not depend on the normal. With either game's normals the lattice gives the same samples on 17 of the 35 hulls worked out in both. BSPGEN's count changed on every one of them, by 8 samples at the least and by hundreds on most. So either BSPGEN's test leans on the normals in some way that this one doesn't, or the BSPGEN of FreeSpace 2 was not quite the same program.

### Earlier findings that the lattice accounts for

- **The error is the same each time.** `check_runs.py` sets the two games' runs on 28 closed hulls against the exact answer and against each other. They agree with each other nine times better than either agrees with the truth.
- **The grid follows the hull and not the scene.** The three asteroids sit 60 metres one way from the origin of their scenes and 70 another, and their errors are alike.
- **The error comes in lumps.** `check_lumps.py` reads what BSPGEN stored, less what is exact, as a lump of material. The lumps are columns turned over.

Before the lattice was found, some thirty schemes were tried against the asteroid and the cargo hulls: grids of rays and lattices of every size and placing, slabs, rays out of the origin, the rules of Simpson and Boole, cells split where a face passes through them, and points from `rand()`. All came near the exact answer and none came nearer to BSPGEN. What they lacked was the drift of the float and the faults of the ray test.

## The hypotheses, as they stand

| Hypothesis | Verdict |
|---|---|
| A proprietary formula | In part. The volume and centroid are the usual ones, as sampled. The tensor isn't |
| Detail levels beyond the first were included | No |
| Debris was included | No |
| Open meshes were handled some other way | No. By the same count of crossings |
| The submodels to include were chosen differently | Yes. The hull alone |
| A bug of its own | Yes, three of them: two in the parallel axis step, and a ray test with no margin |
| The tensor was computed before the faces were removed | No need. Where the hull of the source is closed, the hull of the POF is whole |
| FreeSpace 2 was built by a BSPGEN that worked otherwise | No |

## What this means for pof-tools

pof-tools' own calculation weighs the whole of detail 0's tree, takes the tensor about the model origin, and integrates exactly. That is the sounder calculation, and it is why it comes out a median of 13% from the retail numbers and not closer.

To come within about 0.2% of retail, pof-tools would have to do as `lattice.py` does:

1. take the detail 0 submodel alone, with its polygons as they are in the POF
2. lay the lattice over its bounding box
3. judge each sample by a ray along z, cast by the test of `fvi.py`
4. add up the volume and the nine sums in floats
5. take the mass as `4.65 * volume^0.6667`, the center of mass as the sums of the places over the count, and the tensor as the one about the origin less the volume times `identity - c * cT`, scaled by the mass over the volume and inverted

That reproduces what are very likely three bugs, so it belongs behind a choice of its own and not in place of what is there.

It will not give the retail numbers to the digit. On one model in seven it will be more than 1% out, where a fault falls otherwise than BSPGEN's did.

## What isn't known

1. **Which floats the verts were when the rays were cast.**
2. **What turns a few samples over on the cleanest hulls.**
3. **What changed between the two games,** and why the change in the normals should matter as much as it does.
4. **The order of the two inner loops.**
5. **What `-b` does,** and what the `0xDEAD` chunk holds.
6. **Where the 1 comes from.** One guess: `vm_vector_2_matrix` makes a matrix of rotation from a vector, and such a matrix times its transpose is the identity. Code that built the outer product of `c` that way would have the identity in its place.

## The scripts

They need Python 3 and numpy. Each takes a folder of models, or two, and prints its usage when run with no argument.

| Script | What it answers |
|---|---|
| `p3d.py` | Reads a `.p3d`. Run on a file, prints its chunk tree |
| `pof_header.py` | Reads the header with its cross sections, the submodel headers with their vertices, and the log of a POF. Run on a file, prints them. Run on a folder, counts the versions |
| `pof_bsp.py` | Reads the polygons of a submodel as BSPGEN left them in the POF, and the tree they hang in |
| `fvi.py` | BSPGEN's test of a ray against a polygon, with the arithmetic of the x87 |
| `lattice.py` | BSPGEN's way of weighing a hull: the lattice, the judging of its samples, the sums and the header made from them |
| `models.py` | Shared by the rest: pairs each POF with its source, places the hull in POF coordinates, and integrates a mesh |
| `sampling.py` | Shared by the rest: exact crossings of rays with a mesh, and estimates made from them |
| `bspgen_logs.py` | What did BSPGEN log for each model, and with which flags was it run? |
| `check_volume.py` | Which geometry was weighed? |
| `check_centroid.py` | Is the center of mass the hull's centroid, and which way round are the axes? |
| `check_tensor.py` | Which formula gives the stored tensor? |
| `solve_extra_term.py` | What does the stored tensor hold beyond the central one? |
| `check_fs2.py` | Did FreeSpace 2's BSPGEN work as FreeSpace 1's did? |
| `check_runs.py` | Is the error chance, or the same each time? |
| `check_open_hulls.py` | How was a hull with holes weighed? |
| `check_cross_sections.py` | What are the cross sections in the header, and how did BSPGEN cast a ray? |
| `check_lumps.py` | Where is the error, and where is what changed between the two games? |
| `check_lattice.py` | Does the lattice give what BSPGEN stored? Takes ten minutes |
| `check_pof_alone.py` | How near does it come with nothing but the POF? Takes ten minutes |

```
python check_lattice.py D:/tmp/fs1_pof
python check_pof_alone.py D:/tmp/fs2_pof
```
