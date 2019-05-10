# Exact Function/Curve/Fractal Plotter

A web widget for reliably plotting function graphs, parametric curves and certain fractals.

[Try it now!](http://duck.aston.ac.uk/konecnym/plotter/)

_Beware: The UI and plotting is quite slow due to the cost of reliable arbitrary-accuracy arithmetic running within your browser._

To speed up rendering, tune the plots with very low accuracy and increase accuracy only for the final rendering.

## Examples

<div>
<img src="README-images/xsinrecipx-accurate.png" alt="infinitely many waves" height="200" width="300">
<img src="README-images/umbrella-accurate.png" alt="umbrella-like fractal" height="200" width="300">
</div>

The above images are obtained by drawing shapes that **reliably enclose** the exact objects.
In particular, rounding errors are correctly accounted for.
These graphical enclosures can be computed to an **arbitrarily high accuracy**.  
In the images below the same objects are plotted with a **low accuracy** so that the shapes are easy to see.

<div>
<img src="README-images/xsinrecipx-inaccurate-above-accurate.png" alt="infinitely many waves - low accuracy" height="200" width="300">
<img src="README-images/umbrella-inaccurate-above-accurate.png" alt="umbrella-like fractal - low accuracy" height="200" width="300">
</div>

The following are high and low accuracy plots of an infinitely winding spiral:

<div>
<img src="README-images/spiralInf100.png" alt="infinite spiral - high accuracy" height="180" width="180">
<img src="README-images/spiralInf100-withEnclosure.png" alt="infinite spiral - low and high accuracy" height="180" width="180">
<img src="README-images/spiralInf100-enclosure.png" alt="infinite spiral - low accuracy" height="180" width="180">
</div>

Also, see the [screenshots folder](screenshots) and [slides for a school outreach](regional-cstaster-MK-cid-slides.pdf) featuring plots produced by this tool. 

<!-- Screenshots: -->

