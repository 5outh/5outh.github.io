---
title: A Story of Iteration - Generating Blotch
author: Ben Kovach
date: 2018-04-30
tags: 
  - Haskell
  - Generative
---

### Introduction

At time of writing, "Blotch" is my most recent piece of generative artwork. It
looks nothing like I had originally envisioned, and the iteration of the project
was interesting in its own right. Getting this piece to a place that I actually liked
was a long, fun, and somewhat arduous process punctuated with tons of intermediate
results that look absolutely nothing like the final piece. This isn't uncommon,
and I want to talk about it. Feel free to scroll down to the bottom of this post
or check out my  [art page](/art.html) to see the final result.

## 1
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/001.png"></img>

It all started with a square. I've been playing with a data structure that lets
me index into a square lattice, and I wanted to render a flow field using it.
The basic idea here is that we're shoving a randomly angled unit vector into
each position in the lattice constrained by that central square, then running an
averaging filter over the field many times (averaging each vector with its eight
neighbors). At the end, a bunch of streamlines are drawn through the flow field.
It's a simple idea that I revisit from time to time.

## 2
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/002.png"></img>

Next, I mixed up the rendering a bit by tapering the streamlines.

## 3
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/003.png"></img>

The same idea with much larger (and many fewer) lines.

## 4
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/004.png"></img>

Rendering in black *and* white gives this cool layering effect, but large blobs
don't seem to go anywhere, which was kind of a bummer.

## 5
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/005.png"></img>

Here's a variation on the same idea, clipped into a circle.

## 6
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/006.png"></img>

There are two new things happening here:

First, I moved back to the original line tapering streamline program like <a
href="#section-1">2</a>, but only generated starting points inside of a circle
instead of the original square.

The other, more important (and subtler) point is this: I started plotting fixed
points of the vector field. Those red dots represent locations in the field that
have magnitude less than a certain threshold (which at this point was set really
low).  I originally thought it would be nice to show these inside of the shape
(if you squint, you can see a couple of them inside the circular field). Lines
close to red dots will always be very short, so it's kind of fun to point out
those locations.

**Note:** When I say "fixed point threshold", I mean the value under which a
vector is considered to have 0 magnitude. Plotting only _actual_ zeros would
very likely show nothing.  This variable is
played with a bit, so I want to clarify what I mean.

## 7
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/007.png"></img>

This is exactly the same thing as before with many more lines, generated from
points inside of a set of randomly generated circles within the viewport. I may
explore this further; I like the result looking back. At the time, it wasn't
what I was looking for. Onwards!

## 8
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/008.png"></img>

I noticed a little teardrop pattern in the shorter streamlines, so I generated
some of those a little larger with a small drop shadow.

## 9
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/009.png"></img>

I made them bigger, but still didn't love what I was seeing. At this point, I'd
been seeing bunches of small red dots show up around the flow fields for a
while, and started wondering about something...

## 10
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/011.png"></img>

At this point, I tried removing the streamline rendering altogether and instead
focused my attention on the fixed points. I upped the fixed point threshold (to
include points with a higher magnitude) and just plotted those. I liked the
emerging pattern. This was a big pivot point - everything from here on out is
focused on these fixed points.

## 11
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/012.png"></img>

Another rendering of the same thing with an even higher threshold for fixed
points.

## 12
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/013.png"></img>

The rendering each point was a little boring, so I decided to change it from a
red dot to a black one with varying size. The size of each dot is proportional
to the number of neighbors in the lattice that are also fixed points, which
produces this little pattern.

## 13
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/014.png"></img>

Here's another variation on the same thing; the size of each point is upped a
bit, but the proportions remain the same. The circle fill is replaced with a
stroke. This is pretty close to the final pattern I ended up with.

## 14
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/015.png"></img>

I spent a little while layering these flow field representations with
progressively stricter fixed point thresholds. This one renders a bunch of them
on top of one another, with a thin layer of a low-alpha white painted underneath
it.

## 15
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/016.png"></img>

That looked muddy to me, so I started rendering something similar to <a
href="#section-12">13</a>, but constraining points to a central circle.

## 16
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/017.png"></img>

This is where things started to take shape. Instead of layering the series of
plots on top of one another like in <a href="#section-14">16</a>, I gave the
plots a small vertical offset, which produced a nice pattern.

## 17
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/018.png"></img>

The basis of this whole piece is an algorithm that smooths a two dimensional
space by averaging neighors. Riffing on that idea, you can get an equally nice
one dimensional smoothing algorithm by averaging the neighbors of a list of
values.

That's exactly what I did here: this smooth horizontal movement follows the path
of a list of normally distributed values smoothed in exactly this way.

## 18
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/019.png"></img>

Does it look better as a square? I didn't think so, but it was worth a shot.

## 19
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/020.png"></img>

I run a lot of things that are constrained inside of a shape through a simple
rectangle subdivision algorithm to see how it turns out. I tried that here, but
it didn't add much to the overall composition.

## 20
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/021.png"></img>

Once again, I tried removing the vertical offsets. Looks a bit like a sequence
of stamps to me.

## 21
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/022.png"></img>

Back to the vertical offsets, this time with circles scaled smaller. This was
starting to look like a dead end, so I moved back to circular blotches.

## 22
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/023.png"></img>

This needed color! I'm still getting comfortable with color, but I watched a
short video on HSV color theory which gave me a pretty good idea, which I used
here. I'll explain a bit.

Aaron Rutten [posted a YouTube video](https://youtu.be/8UGleDu1KEo?t=614) a
while ago about using HSV to shade and tint. He describes a system of shading
which involves drawing a "curve" on a color triangle, starting with low
saturation and high brightness (lights), curving over to high saturation
and medium brightness (colorful), then back over to low saturation/low
brightness (darks). This gave me an idea. Quadratic Bezier curves are defined
with two end points and a control point; three points that can be tweaked to
produce exactly the type of curve that Aaron mentions in his YouTube video. In
order to produce a color for each successive blotch, I tried sampling this
Bezier curve for a purple color, starting with a dark color and ending with a
light one. I'll refer to this as a "color Bezier" from now on.

## 23
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/024.png"></img>

This was nice, but still needed another element. Why not outline the circles
behind each blotch? I tried it, and it really didn't work.

## 24
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/025.png"></img>

Once again, I tried removing the vertical offsets. This looked to me a little
like a texture from an old video game.

## 25
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/026.png"></img>

Next, I went back to something like <a href="#section-21">22</a>, with three tweaks:

1. Randomly sample the color Bezier for colors instead of interpolating along it
   for colors
2. Draw the blotches in reverse order, so the one nearest the top shows up
   layered above the ones below it.
3. Instead of only considering immediate neighbors to determine circle size, use
   the 5x5 grid centered at the point.

In the final piece, (3) is still used. I think it produces a more interesting
pattern.

## 26
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/027.png"></img>

I felt like this wasn't going anywhere at this point and almost gave up on the
whole thing. I thought hard about what this piece needed, and decided to try
slicing the blotches randomly along their midpoint and layering them to try to
introduce more variation. The color gradient is back; I didn't like the random
sampling of colors.

## 27
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/028.png"></img>

The last one was a little too chaotic. Instead, I tried only slicing near-vertically.

## 28
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/029.png"></img>

Tried using a different color to produce the color Bezier, and got this blueish
one. I also moved back to the random slices. I liked the color of this.

## 29
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/030.png"></img>

**Yet again** I removed the vertical offsets (or, at least lowered them). This
produced a pretty cool gradient effect this time, but didn't fill the space
well.

## 30
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/031.png"></img>

At this point, I thought that something similar to <a href="#section-27">28</a> was close, but
lacking something. I added a brushstroke pattern by dragging a set of points
generated in a small circle across a randomly constructed Chaikin curve in the
background. It looked really out of place, so I removed it.

## 31
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/032.png"></img>

I went back to something like <a href="#section-26">27</a>, but this time tried to produce some constrast
by splitting the space into two separate vertical slices and rendering them with
complementary color Beziers.

## 32
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/033.png"></img>

There's a bit of a leap here. I didn't like the last few images, and felt I was
going in circles. Two things I wanted to improve were:

1. There was too much going on, and lots of details were being lost.
2. The colors really weren't popping. This is *really* frustrating to me as
   someone learning to use color effectively!

To address these issues, I made two significant changes. First, I reduced 
the number of iterations of blotches to only 5, instead of the previous 20ish
(can't remember the exact number), which dealt with the chaos and made the
shapes pop a bit better. To deal with the color issue, I made a somewhat
unintuitive change. I started generating the color Bezier from not one, but two,
_completely random_ hues, linearly interpolating between the two hues as the
Bezier is constructed. Because I know the *system* works, I could sort of get
away with leaving up the color generation to randomness and forget about hand
picking them. A lot of the results around this time weren't colored super well,
but some, like this one, turned out quite nicely.

## 33
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/034.png"></img>

Here's another iteration from around this time. I liked this one enough to share
with others as a work in progress. I still thought it was missing something, but
the colors were nice.

## 34
<img style="margin-left:auto; margin-right:auto; display:block;" width="70%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/035.png"></img>

I hated the one background I tried back in <a href="#section-30">31</a>, but still thought the piece
needed a background. After a while, I realized I aready had something that
meshed well with the foreground: the original vector field! I started rendering
each vector in the field with a small line width, which produced a nice, faint
texture like I was looking for.

## 35
<img style="margin-left:auto; margin-right:auto; display:block;" width="80%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/036.png"></img>

The border of the vector field was still a bunch of untouched, random vectors,
which formed a noisy edge. To fix that up, I scaled the vector field to contain
a few more lattice points outside of the viewport so the outer edges aren't
visible.

## 36 (Final)
<img style="margin-left:auto; margin-right:auto; display:block;" width="80%" src="https://ben-kovach-blog-assets.s3.amazonaws.com/images/blotch/037.png"></img>

I liked the previous result, but it needed small tweaks. If you aren't looking
super carefully, these two images probably look identical. The changes made here
are some minor tweaks to the fixed point thresholds, and a soft glow behind each
cluster of circles, which helps the layers stand out. It's worth mentioning that
my process changed a little bit here. Instead of generating random images, I
found my candidate and began re-generating the same exact image with tweaks that
didn't mess with the random generator. This has been a useful pattern for me,
and I recommend giving it a try.

## In conclusion...

I didn't have much of an end goal at the start of this process other than "make
something cool using this data structure I have on hand." Half of the process of
generating art  for me is taking every idea that pops up and running with it,
and the other half is evaluating and criticizing the results that show up. The
images shown here are only a small subset of the images generated along the way,
but they serve as a good representation of the path the piece took from start to
finish.

For more artwork like this, see [my art page](/art.html) and follow me on
[twitter](twitter.com/bendotk) and [instagram](instagram.com/bendotk). "Blotch"
is available as a one-of-a-kind print on [my shop](https://bendotk.bigcartel.com).

Finally, if you want to be alerted of new art for sale, feel free to sign up for
my newsletter for the occasional update. I won't spam you, I promise!

<link href="//cdn-images.mailchimp.com/embedcode/horizontal-slim-10_7.css" rel="stylesheet" type="text/css">
  <style type="text/css">
          #mc_embed_signup{background:#fff; clear:left; font:14px Helvetica,Arial,sans-serif; width:100%;}
          /* Add your own MailChimp form style overrides in your site stylesheet or in this style block.
             We recommend moving this block and the preceding CSS link to the HEAD of your HTML file. */
  </style>
  <div id="mc_embed_signup">
  <form action="https://kovach.us12.list-manage.com/subscribe/post?u=126021f20d39147e20047aa2a&amp;id=f84bdb0f0d" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_blank" novalidate>
  <div id="mc_embed_signup_scroll">
  <!--<label for="mce-EMAIL">Subscribe to newsletter</label>-->
  <input type="email" value="" name="EMAIL" class="email" id="mce-EMAIL" placeholder="email address" required>
  <!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
  <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_126021f20d39147e20047aa2a_f84bdb0f0d" tabindex="-1" value=""></div>
  <div class="clear"><input type="submit" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button"></div>
  </div>
  </form>
</div>

Thanks for reading! I would be happy to expand on any individual points brought
up during this whirlwind tour of my process. Feel free to <a
href="/contact.html">contact me</a> for further clarification.

Ben
