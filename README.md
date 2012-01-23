Napa-FFT3: Overview
===================

Napa-FFT3 is a complete rewrite of Napa-FFT (version 2 is an aborted
experiment).  The goal is still the same: to provide, via a mixture of
cache-friendly algorithms and code generation, FFT routines in Common
Lisp that offer performance comparable to the state of the art.  In
that regard, it is a success: depending on how it's used, Napa-FFT3
is, at most, around three times as slow as FFTW on small or medium
inputs, and can be faster than FFTW for large inputs.  The complete
picture is more complicated than this; see the _Performance_ section
for details.

The goal of Napa-FFT3 isn't only to provide Discrete Fourier Transform
(DFT) routines, but also (rather) to provide buildings blocks to
express common operations that involve DFTs: filtering, convolutions,
etc.  This is what enables Napa-FFT to achieve such high performance
without optimizing at the assembly level.  The _Easy Interface_
section should suffice for most developers; the _Low-level Interface_
is described in another section, and may be of interest to some.

Napa-FFT3 also expressly supports FFTs on real data and inverse FFTs
back to real data.  The _Real Interface_ section describes the
facility, and is used in conjunction with the _Easy Interface_.

Finally, see the _Installation_ section for installation instructions,
and the _Implementation_ section for all the gory details.

Note that Napa-FFT3 currently only supports power-of-two-sized inputs;
even when/if it will gain code for arbitrary sizes, powers of two will
most likely be much more efficient, both in terms of runtime and space
usage.

To recapitulate:

 * Installation: installation instructions;
 * Easy Interface: convenience functions;
 * Real Interface: convenience functions for real-only input or
   output;
 * Examples: more examples;
 * Low-level Interface: unsafe access to low-level facilities;
 * Implementation: details on algorithms, the code generation process,
   etc.
 * Performance: benchmarks.
 
Installation
------------

Napa-FFT3 is a regular ASDF system defined in `napa-fft3.asd`.  If
Quicklisp is installed, it suffices to copy the Napa-FFT3 directory
under `~/quicklisp/local-projects`.

Once registered with ASDF, Napa-FFT3 can be loaded by executing
`(asdf:oos 'asdf:load-op "napa-fft3")`, or, with Quicklisp, 
`(ql:quickload "napa-fft3")`.

Easy Interface
--------------

The core of the "easy" interface consists of:

 * `NAPA-FFT:FFT`: forward DFT
 * `NAPA-FFT:IFFT`: inverse DFT
 * `NAPA-FFT:BIT-REVERSE`: bit-reversal routine
 * `NAPA-FFT:WINDOWED-FFT`: windowed forward DFT
 * `NAPA-FFT:WINDOWED-IFFT`: windowed inverse DFT

### FFT

Syntax: `fft vec &key dst size in-order scale window => vector`.

Arguments and Values:

 * _vec_: sequence of samples.
 * _dst_: nil (default) or a simple vector of complex samples
   (destructively reused).
 * _size_: size of the transform to perform (must be a power of
   two). `(length vec)` if nil (default).
 * _in-order_: whether the result should be in-order (default, t) or
   bit-reversed (nil).
 * _scale_: how the result should be scaled: not at all (default,
   nil), by _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (t,
   or :inv).
 * _window_: simple array of double or complex doubles by which the
   input is multiplied point-wise; no multiplication if nil
   (default).
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.
   
`FFT` computes the DFT of the first _size_ values in _vec_.

First, _vec_ is converted to a simple array of complex samples if
necessary.  The result is stored in _dst_, or a fresh array of complex
doubles. _dst_ may be the same object as _vec_ for an in-place
transform.

If _window_ is non-nil, each value in _vec_ is multiplied by the
corresponding value in _window_ during the transform; similarly, the
values are scaled according to the value of _scale_.

If _in-order_ is true, the result is then converted to be in order,
which can take more than half as much time as the FFT itself.

Example:

    CL-USER> (napa-fft:fft '(0 1 2 3 4 5 6 7))
    #(#C(28.0d0 0.0d0) #C(-4.0d0 9.65685424949238d0) #C(-4.0d0 4.0d0)
      #C(-4.0d0 1.6568542494923806d0) #C(-4.0d0 0.0d0)
      #C(-4.0d0 -1.6568542494923806d0) #C(-4.0d0 -4.0d0)
      #C(-4.0d0 -9.65685424949238d0))
    
    ;; the same, but bit reversed
    CL-USER> (napa-fft:fft '(0 1 2 3 4 5 6 7) :in-order nil)
    #(#C(28.0d0 0.0d0) #C(-4.0d0 0.0d0) #C(-4.0d0 4.0d0) #C(-4.0d0 -4.0d0)
      #C(-4.0d0 9.65685424949238d0) #C(-4.0d0 -1.6568542494923806d0)
      #C(-4.0d0 1.6568542494923806d0) #C(-4.0d0 -9.65685424949238d0))

    ;; :scale nil is the default
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale nil)
    #(#C(6.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 0.0d0) #C(-2.0d0 -2.0d0))
    
    ;; the same, but scaled by 1/4
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale t)
    #(#C(1.5d0 0.0d0) #C(-0.5d0 0.5d0) #C(-0.5d0 0.0d0) #C(-0.5d0 -0.5d0))
    
    ;; again, scaled by 1/sqrt(4) = 1/2
    CL-USER> (napa-fft:fft '(0 1 2 3 5 6 7 8) :size 4 :scale :sqrt)
    #(#C(3.0d0 0.0d0) #C(-1.0d0 1.0d0) #C(-1.0d0 0.0d0) #C(-1.0d0 -1.0d0))


### IFFT

Syntax: `ifft vec &key dst size in-order scale window => vector`.

Arguments and Values:

 * _vec_: sequence of Fourier coefficients.
 * _dst_: nil (default) or a simple vector of complex samples
   (destructively reused).
 * _size_: size of the transform to perform (must be a power of
   two). `(length vec)` if nil (default).
 * _in-order_: whether the result should be in-order (default, t) or
   bit-reversed (nil).
 * _scale_: how the result should be scaled: not at all (nil), by
   _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (default, t or :inv).
 * _window_: simple array of real or complex sample by which the
   out-of-order input samples are multiplied elementwise.  If nil
   (default), no multiplication is performed.
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.
   
`IFFT` computes the inverse DFT of the first _size_ Fourier
coefficients in _vec_.

First, _vec_ is converted to a simple array of complex samples if
necessary.  The result is stored in _dst_, or a fresh array of complex
doubles. _dst_ may be the same object as _vec_ for an in-place
transform.

If _in-order_ is true, the result is then converted to be
bit-reversed, which can take more than half as much time as the FFT
itself.

If _window_ is non-nil, each value in _vec_ is multiplied by the
corresponding value in _window_ during the transform; similarly, the
values are scaled according to _scale_.  Note that this happens
_after_ the bit-reversal.  _window_ should thus be bit-reversed
itself.  Since this corresponds to a convolution, this is usually
easily satisfied.

Example:

    ;; the defaults ensure fft and ifft are inverses
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3)))
    #(#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0) #C(3.0d0 0.0d0))
    
    ;; Skipping both bit-reversals saves a lot of time, without
    ;; changing the result; similarly, scaling can happen at any
    ;; step.
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3) :in-order nil
                                      :scale :sqrt)
                        :in-order nil
                        :scale :sqrt)
    #(#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0) #C(3.0d0 0.0d0))
    
    ;; The :window argument performs convolutions; simply make
    ;; sure that :in-order is nil when transforming the convolved
    ;; vector.  Here, the input is shifted one element to the
    ;; right and scaled by 1/2.
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3))
                            :window (napa-fft:fft '(0 1/2 0 0)
                                                  :in-order nil))
    #(#C(1.5d0 0.0d0) #C(0.0d0 0.0d0) #C(0.5d0 0.0d0) #C(1.0d0 0.0d0))

### BIT-REVERSE

Syntax: `bit-reverse vec &optional dst size => vector`.

Arguments and values:

 * _vec_: array of complex or real samples to bit-reverse.
 * _dst_: nil, or the destination array of the same type as _vec_.
 * _size_: number of elements to reorder. If nil, defaults to the
   size of _vec_. Must be a power of two.
 * _vector_: bit-reversed permutation of _vec_.
 
`BIT-REVERSE` permutes the first _size_ elements in _vec_ to
bit-reversed indices, storing the result in _dst_ if provided.  _vec_
may be the same as _dst_ for an in-place reversal.

It should usually not be necessary to bit-reverse values explicitly,
but it may still be useful to convert an in-order vector to
out-of-order or vice-versa.

Example:

    CL-USER> (napa-fft:bit-reverse (coerce '(0d0 1d0 2d0 3d0)
                                           'napa-fft:real-sample-array))
    #(0.0d0 2.0d0 1.0d0 3.0d0)
    
    ;; bit-reverse is its own inverse.
    CL-USER> (napa-fft:bit-reverse * *)
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### WINDOWED-FFT

TODO. It's the same as Bordeaux FFT.

### WINDOWED-IFFT

Real Interface
--------------

The real interface offers three functions specialized to operate on
real (not complex) data:

 * `NAPA-FFT:RFFT` performs in-order real-input FFTs.
 * `NAPA-FFT:RIFFT` performs in-order real-output inverse FFTs.
 * `NAPA-FFT:WINDOWED-RFFT` performs windowed in-order real-input
   FFTs.
 * `NAPA-FFT:WINDOWED-RIFFT` performs windowed in-order real-output
   inverse FFTs.

These are convenient because the result is a vector of real values,
but also offer strong performance improvements (almost halving
computation times) for in-order, out-of-place, transforms, at the
expense of a little precision.

### RFFT

Syntax: `rfft vec &key dst size scale => vector`

Arguments and values:

 * _vec_: sequence of real samples.
 * _dst_: nil, or a simple vector of complex doubles (destructively
   reused).
 * _size_: size of the transform to perform. Defaults to `(length vec)`.
   Must be a power of two.
 * _scale_: how the result should be scaled: not at all (default,
   nil), by _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (t,
   or :inv).
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.

`RFFT` computes the in-order DFT of the first _size_ samples in _vec_.

_vec_ is converted, if necessary, to a simple array of doubles.  Its
DFT is then re-expressed as a half-size DFT of complex samples, and
the result is written in _dst_.

Example:

    ;; rfft should always yield the same result as fft (modulo
    ;; rounding)
    CL-USER> (napa-fft:rfft '(0 1 2 3))
    #(#C(6.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 0.0d0)
      #C(-1.9999999999999998d0 -2.0d0))


### RIFFT

Syntax: `rifft vec &key dst size scale => vector`

Arguments and values:
 * _vec_: sequence of complex Fourier coefficients. Destroyed.
 * _dst_: nil, or a simple vector of doubles. Destructively reused.
 * _size_: size of the inverse transform to perform. Defaults to
   `(length vec)`.  Must be a power of two.
 * _scale_: how the result should be scaled: not at all (nil), but
   _1/sqrt(size)_ (:sqrt or sqrt) or by _1/n_ (default, t or :inv).
 * _vector_: a simple array of doubles. _dst_ if not nil, otherwise a
   newly-allocated array.

`RIFFT` computes the in-order inverse DFT of the first _size_ Fourier
coefficients in _vec_.

_vec_ is converted, if necessary, to a simple array of complex
doubles.  Its inverse DFT is then re-expressed, destructively as a
half-size DFT of complex doubles.  The procedure can only work
correctly when the output is purely real: even if we're only
interested in the real component, it will be distorted by any non-zero
imaginary value.

Example:

    CL-USER> (napa-fft:rifft (napa-fft:rfft '(0 1 2 3)))
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### WINDOWED-RFFT

Same.

### WINDOWED-RIFFT

...

Examples
--------

I'm no good at this signal processing stuff; most of what I know about
this comes, indirectly, from classical music training.

I'll use sox to play sound files easily, and the following function to
convert float samples to `s32` files:

    (defun emit-raw32-file (file data &optional (max 1d0))
      (with-open-file (s file :direction :output :element-type '(signed-byte 32)
                              :if-exists :supersede)
        (write-sequence (map-into (make-array (length data)
                                              :element-type '(signed-byte 32))
                                  (lambda (data)
                                    (let ((x (/ (float (realpart data) 1d0)
                                                max)))
                                      (floor (* x (1- (ash 1 31))))))
                                  data)
                        s)
        file))

Napa-FFT3 does a fair amount of runtime compilation and cached
computations.  If an operation is slow (more than one second), it was
almost certainly due to a slow one-time operation; try running it
again.  Also, there's a lot of gratuitous consing, here.  Nearly all
the operations could be in-place and (nearly) non-consing without any
change to the code.

### Generate a single tone

The standard frequency of the middle A is 440 Hz nowadays.  Sound is
commonly generated, on computers, at 44100 Hz.  Let's generate a
signal of 64k points that, when played back at 44100 Hz, will result
in an A440.  The following function returns a vector with zeros
everywhere except where specified in the first argument, a list
designator of indices.

    (defun impulse (i n &optional (value 1d0))
      (let ((vec (make-array n :element-type 'napa-fft:complex-sample
                               :initial-element (complex 0d0 0d0))))
        (dolist (i (if (listp i) i (list i)) vec)
          (setf (aref vec i) (complex (float value 1d0))))))

The DFT computes a vector such that each entry corresponds to the
amount of energy in the frequency(ies) corresponding to that entry.
The _i_ th entry of an _N_ -element Fourier-coefficient vector, with an
original sampling frequency of _F_ corresponds to a signal frequency
of _iF/N_ (+/- _F/N_ ).  So, if we want a wave at 440 Hz in in a
vector of 64k played back at 44100 Hz, we have to put energy in the
_(440/44100)*65536_ th bin.

    ;; create a vector with 1 only in the bin corresponding to 440 Hz,
    ;; convert back to the time domain, and save the double values as
    ;; a file of (signed-byte 32) values.
    CL-USER> (emit-raw32-file "~/napa-fft3/example/a440.s32"
                              (napa-fft:ifft (impulse (round (* 440 65536)
                                                             44100)
                                                      65536)
                                             :scale nil))
    "~/napa-fft3/example/foo.s32"

    $ play -r44100 a440.s32 # play is a sox command; play a file of
                            # signed 32 bit samples at 44100 Hz.
                            # Should sound like an A440!

### Generate a chord

We (westerners) are mostly used to a scale system based on 12
equispaced (not quite, but close enough) semi-tones.  Reality tends to
enforce that the scale covers a range of frequences for _F_ to _2F_.
For example, while the middle A is at 440 Hz, the one in the next
octave is at 880 Hz.  The middle C is 9 half-tones lower than the
middle A, at `(* 440 (expt .5 (/ 9 12)))`, around 262 Hz (264 Hz in
the real world).  E is then 2 tones higher, at `(* 262 (expt 2 (/ 4 12)))`
~= 330 Hz, and G a tone and a half higher again, `(* 330 (expt 2 (/ 3 12)))`
~= 392 Hz.

Thus, to hear the boring middle C/E/G chord, we need energy at 262,
330 and 392 Hz; note how the energy is 1/3 at each point, so that the
total comes to 1.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/chord.s32"
                              (napa-fft:ifft (impulse (list (round (* 262 65536)
                                                                   44100)
                                                            (round (* 330 65536)
                                                                   44100)
                                                            (round (* 392 65536)
                                                                   44100))
                                                      65536
                                                      (/ 3d0))
                                             :scale nil))
    "~/napa-fft3/example/foo.s32"
    
    $ play -r44100 chord.s32 # you should recognize this sound

### Filter frequencies out

First, let's save our time-domain chord signal:

    CL-USER> (defparameter *chord* (napa-fft:ifft
                                    (impulse (list (round (* 262 65536) 44100)
                                                   (round (* 330 65536) 44100)
                                                   (round (* 392 65536) 44100))
                                             65536
                                             (/ 3d0))
                                    :scale nil))
    *CHORD*

Here's a function to generate random noise and another to average two vectors:

    (defun noise (n &optional (range .5d0))
      (let ((2range (* 2 range)))
        (map-into (make-array n :element-type 'napa-fft:complex-sample)
                  (lambda ()
                    (complex (- (random 2range) range))))))
    
    (defun m+ (x y &optional (scale .5d0))
      (map 'napa-fft:complex-sample-array
           (lambda (x y)
             (* scale (+ x y)))
           x y))

We can noise our signal up by adding noise to the chord:

    CL-USER> (defparameter *noisy-chord* (m+ *chord* (noise 65536)))
    *NOISY-CHORD*
    
    $ play -r44100 noised-chord.s32 # still recognizable, but
                                    # annoying.

Let's say that we know that the only interesting stuff is in the
central octave.  We could zero out all the frequencies outside the
262-524 Hz range.

First, we have to convert the noisy signals in the frequency domain:

    CL-USER> (defparameter *noisy-chord-freq* (napa-fft:fft *noisy-chord*))
    *NOISY-CHORD-FREQ*

Then, we want to replace everything outside `(round (* 262 65536) 44100)` and
`(round (* 524 65536) 44100)` with 0:

    CL-USER> (defparameter *octave-chord-freq* (copy-seq *noisy-chord-freq*))
    *OCTAVE-CHORD-FREQ*
    CL-USER> (prog1 nil
               (fill *octave-chord-freq* (complex 0d0)
                     :end (round (* 262 65536) 44100))
               (fill *octave-chord-freq* (complex 0d0)
                     :start (1+ (round (* 524 65536) 44100))))

Now, we can convert back in the time domain and listen to the result:

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord.s32"
                              (napa-fft:ifft *octave-chord-freq*))

Much better.

#### Filter frequencies out in a real signal

Note that, as is often the case, we know that our signal is real (has
no imaginary component).  In this case, we can use real-only FFT/IFFT
instead:

    CL-USER> (defparameter *noisy-chord-freq*
               (napa-fft:rfft (map 'napa-fft:real-sample-array
                                   #'realpart
                                   *noisy-chord*)))
    *NOISY-CHORD-FREQ*
    CL-USER> (prog1 nil
               (fill *octave-chord-freq* (complex 0d0)
                     :end (round (* 262 65536) 44100))
               (fill *octave-chord-freq* (complex 0d0)
                     :start (1+ (round (* 524 65536) 44100))))
    NIL
    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord.s32"
                              (napa-fft:rifft *octave-chord-freq*))

The result is the same, but (once the routines are compiled), each
FFT/IFFT is about twice as fast.  Mind the fact that rifft is
destructive on its input, however.  It also only works when the output
is purely real; any non-zero imaginary component will warp the
real-valued output.

#### Filter frequencies out during the IFFT

We can do this directly, by filtering during the inverse FFT.
Replacing most values with 0 and leaving the rest along is equivalent
to multiplying by 0 or 1 (usually, we want a more gradual dampening,
and the filter will also have fractional values).

    CL-USER> (defun central-octave-filter (i n)
               (if (<= (round (* 262 n) 44100)
                       i
                       (round (* 524 n) 44100))
                   1 0))
    CENTRAL-OCTAVE-FILTER
    CL-USER> (defparameter *filter*
               (napa-fft:window-vector 'central-octave-filter
                                       65536))
    *FILTER*
    CL-USER> (defparameter *filtered-noisy-chord-freq*
               (map 'napa-fft:complex-sample-array #'*
                    *filter* *noisy-chord-freq*))
    *FILTERED-NOISY-CHORD-FREQ*
    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord2.s32"
                              (napa-fft:ifft *filtered-noisy-chord-freq*))
    "~/napa-fft3/example/octave-chord.s32"

And we have the same final result.  Obviously, an advantage is that we
can compute the filter once, and easily apply it directly.

Another advantage is that we can bit-reverse (permute) the filter
instead of bit-reversing after the FFT and before the IFFT.  Again,
the final result is the same, but we only bit-reverse the (cached)
filter vector instead of many frequency domain vector.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord3.s32"
                              (napa-fft:windowed-ifft 
                               (napa-fft:fft *noisy-chord* :in-order nil)
                               :window-fn 'central-octave-filter
                               :in-order nil))
    "~/napa-fft3/example/octave-chord3.s32"

### Filter low-amplitude noise out

In the previous example we already knew at what frequency the
interesting signal was.  That's sometimes the case (e.g. when some of
us unconsciously filter out annoyingly-high-pitched voices), but
somewhat uncommon.

Instead of applying a constant filter on frequencies, we can attempt
to find which frequencies dominate (have a high energy), and eliminate
the rest.

#### Filter by strength

The easiest way is to find the frequency with the most energy, and
clamp out everything that's below a fixed fraction (e.g. 1%) of that.

    (defun amplitude-clamp-fraction (vector fraction)
      (let ((limit (* (reduce #'max vector :key #'abs)
                      fraction)))
        (map 'napa-fft:complex-sample-array
             (lambda (x)
               (if (< (abs x) limit)
                   (complex 0d0)
                   x))
             vector)))

This function performs the same operation, regardless of the order in
which its data is permuted.  We can thus use out-of-order FFTs and
IFFTs here as well.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord4.s32"
                              (napa-fft:ifft
                               (amplitude-clamp-fraction
                                (napa-fft:fft *noisy-chord* :in-order nil)
                                1d-2)
                               :in-order nil))
    "~/napa-fft3/example/octave-chord4.s32"

When frequencies below 1% of the max energy are zeroed out, the amount
of noise is significantly reduced, but there's still an annoying
ringing.  Increasing the limit to 2% improves matters a lot; however,
if we set too high a limit, we'll start to filter out interesting, if
somewhat weak, sounds.

#### Filter by count

We could also know ahead of time that there are only a few (e.g. 3)
interesting frequencies in the signal.  In that case, we can find the
k frequencies with the most energy, and remove everything else:

    (defun amplitude-clamp-k (vector k)
      (let* ((n      (length vector))
             (values (make-array n)))
        (dotimes (i n)
          (setf (aref values i) (cons (abs (aref vector i)) i)))
        ;; this would be a heap or a quickselect if I cared
        (sort values #'> :key #'car)
        (let ((result (make-array n
                                  :element-type 'napa-fft:complex-sample
                                  :initial-element (complex 0d0))))
          (loop repeat k
                for (nil . i) across values
                do (setf (aref result i) (aref vector i))
                finally (return result)))))

Again, the input may be permuted without changing the result, so we
can stick to out-of-order transforms:

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord5.s32"
                              (napa-fft:ifft
                               (amplitude-clamp-k
                                (napa-fft:fft *noisy-chord* :in-order nil)
                                3) ; we're cheating here (:
                               :in-order nil))
    "~/napa-fft3/example/octave-chord5.s32"

### Windowing chunks of long signals

Let's filter Justin Bieber out of one of his songs.  That's actually a
very difficult task, especially for example code: voices tend to have
rich harmonics, and span a wide range of frequencies.  What we can do
is filter every frequency higher than a certain limit.

First, I use sox to convert the input (in wave format) to mono s32:

    $ sox never.wav -r 44100 -c 1 never.s32

I can now read the first minute and convert it to doubles:

    (defun read-raw32-file (file &optional n (max 1d0))
      (with-open-file (s file :element-type '(signed-byte 32))
        (let* ((n   (or n
                        (file-length s)))
               (seq (make-array n :element-type '(signed-byte 32))))
          (read-sequence seq s)
          (let ((scale (expt (* 2d0 max) -31)))
            (declare (type double-float scale))
            (map 'napa-fft:real-sample-array
                 (lambda (x)
                   (* x scale))
                 seq)))))

    CL-USER> (defparameter *never* (read-raw32-file "~/napa-fft3/example/never.s32"
                                                    (* 44100 60)))
    *NEVER*


We could just transform the whole 60 seconds at once, and revert it,
but that'd be a lot of work.  Instead, we'll chunk it into short
periods, say one tenth of a second, and filter each chunk separately.
We'll simplify things and use chunks of 4096 samples (slightly less
than 4410 samples).

Some fiddling around lets us discover than Bieber seems to sing around
260 Hz and higher, so we'll cut off frequencies from 260 Hz and up.

    CL-USER> (defparameter *jb-filter*
               (napa-fft:window-vector (lambda (i n)
                                         (if (< i (round (* 260 n) 44100))
                                             1 0))
                                       4096))
    *JB-FILTER*

The following function will apply the same filter to each chunk of samples.

    (defun filter-chunks (vector filter)
      (declare (type napa-fft:real-sample-array vector filter))
      (let* ((destination (make-array (length vector)
                                      :element-type 'napa-fft:complex-sample))
             (chunk-size  (length filter))
             (chunk       (make-array chunk-size
                                      :element-type 'napa-fft:complex-sample))
             (filter      (napa-fft:bit-reverse filter)))
        (declare (optimize speed))
        (loop for i below (length vector) by chunk-size
              for end = (min (length vector) (+ i chunk-size))
              do (fill chunk (complex 0d0))
                 (replace chunk vector :start2 i :end2 end)
                 (napa-fft:fft chunk :dst chunk :in-order nil)
                 (napa-fft:ifft chunk
                                :dst chunk :in-order nil
                                :window filter)
                 (replace destination chunk :start1 i :end2 end)
              finally (return destination))))

We can then directly filter all the high frequencies in `*never*`.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/never-prime.s32"
                              (filter-chunks *never* *jb-filter*))
    "~/napa-fft3/example/never-prime.s32"

The filtering process loses a lot of volume.  We can recover that by
scaling the output so that its 2-norm is the same.  There's also an
annoying noise: that's an artefact of the way we cut our signal up and
pretend each chunk is periodic.  The latter problem is what the
windowing support attempts to address.

Instead of filtering each chunk independently, we'll use a triangular
window, and overlap the chunks so that each sample is processed
exactly twice.  The triangular window ensures that the two weights
assigned to each sample adds to 1.  `windowed-fft` takes care of
applying the triangular window to the input, and a scaling factor is
applied so that the energy in the output is _half_ that in the input.
The result of the thunks are then added together, with the same
overlap as the input.  `filter-chunks` becomes:

    (defun filter-chunks (vector filter)
      (declare (type napa-fft:real-sample-array vector filter))
      (let* ((destination (make-array (length vector)
                                      :element-type 'napa-fft:complex-sample
                                      :initial-element (complex 0d0)))
             (chunk-size  (length filter))
             (half-size   (truncate chunk-size 2))
             (chunk       (make-array chunk-size
                                      :element-type 'napa-fft:complex-sample))
             (filter      (napa-fft:bit-reverse filter)))
        (declare (optimize speed))
        (loop for i below (length vector) by half-size
              for end = (min (length vector) (+ i chunk-size))
              do (fill chunk (complex 0d0))
                 (loop for dst upfrom 0
                       for src from i below end
                       do (setf (aref chunk dst) (complex (aref vector src))))
                 (let ((old-energy (energy chunk)))
                   (napa-fft:windowed-fft chunk half-size
                                          chunk-size
                                          :window-fn 'napa-fft:triangle
                                          :dst chunk :in-order nil)
                   (napa-fft:ifft chunk
                                  :dst chunk :in-order nil
                                  :window filter)
                   (let* ((new-energy (energy chunk))
                          (scale      (* .5d0 (sqrt (/ old-energy
                                                       new-energy)))))
                     (declare (type double-float scale))
                     (loop for src upfrom 0
                         for dst from i below end
                         do (incf (aref destination dst)
                                  (* scale (aref chunk src))))))
          finally (return destination))))

When I play that back, I only hear high quality percussions and little
to no artefacts.

### Multiplying integers or polynomials via a convolution

So far, we've been using point-wise multiplication in the frequency
domain to filter frequencies out.  We can also see it as a nice way to
execute convolutions.  We can use this to implement fast
multiplication of polynomials or of integers, with the right encoding.
However, please don't use it for bignum multiplications without
checking the precision of the transforms.

Let's multiply 1005 by 1234. 1005 is 1\*10^3 + 0\*10^2 + 0\*10^1 +
5\*10^0, which we can encode, as a vector: #(1 0 0 5 0 0 0 0) (note
the padding at the end), and similarly for 1234.  Again, filtering is
oblivious to any permutation (as long as the windowing vector is
bit-reversed), so we can do everything out of order.

    CL-USER> (napa-fft:fft #(1 0 0 5 0 0 0 0) :in-order nil)
    #(#C(6.0d0 0.0d0) #C(-4.0d0 0.0d0) #C(1.0d0 5.0d0) #C(1.0d0 -5.0d0)
      #C(-2.5355339059327378d0 -3.5355339059327378d0)
      #C(4.535533905932738d0 3.5355339059327378d0)
      #C(4.535533905932738d0 -3.5355339059327378d0)
      #C(-2.5355339059327378d0 3.5355339059327378d0))
    CL-USER> (napa-fft:fft #(1 2 3 4 0 0 0 0) :in-order nil)
    #(#C(10.0d0 0.0d0) #C(-2.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 -2.0d0)
      #C(-0.41421356237309515d0 -7.242640687119286d0)
      #C(2.414213562373095d0 1.2426406871192857d0)
      #C(2.414213562373095d0 -1.2426406871192857d0)
      #C(-0.41421356237309515d0 7.242640687119286d0))
    CL-USER> (napa-fft:ifft * :window ** :in-order nil)
    #(#C(0.9999999999999982d0 0.0d0) #C(1.9999999999999991d0 0.0d0)
      #C(3.0d0 0.0d0) #C(9.0d0 0.0d0) #C(10.000000000000002d0 0.0d0)
      #C(15.0d0 0.0d0) #C(20.0d0 0.0d0) #C(-8.881784197001252d-16 0.0d0))

If we remove the imaginary portions (which are all 0) and round some
numerical errors away, we find #(1 2 3 9 10 15 20 0), this time with
only one element of padding; this value represents 1\*10^6 + 2\*10^5 +
3\*10^4 + 9\*10^3 + 10\*10^2, etc.  If we take care of the carries, we
find 1240170, which is indeed 1005 \* 1234.

Of course, we can also exploit the fact that the input and output are
all reals to use `rfft` and `rifft`.  We can do even better with
`%2rfft`, which performs 2 real fft at the same time.  However, if we
do that, we have to use in-order transforms and perform the
element-wise multiplication ourselves.  It's a trade off, and even
when we're only concerned with computation times, the right answer
depends on a lot of variables.

    ;; the results are returned one after the other in a single
    ;; vector of complex doubles
    CL-USER> (napa-fft:%2rfft '(1 0 0 5 0 0 0 0)
                              '(1 2 3 4 0 0 0 0))
    #(#C(6.0d0 0.0d0) #C(-2.5355339059327378d0 -3.5355339059327378d0)
      #C(1.0d0 5.0d0) #C(4.535533905932738d0 -3.535533905932738d0)
      ...)
      
    CL-USER> (let ((x (subseq * 0 8))
                   (y (subseq * 8)))
               (napa-fft:rifft (map-into x #'* x y)))
    #(0.9999999999999991d0 2.0d0 2.9999999999999964d0 9.0d0 10.0d0 15.0d0
      20.000000000000004d0 -8.881784197001252d-16)

Low-level Interface
-------------------

These functions directly expose the runtime code generator to let you
avoid all the argument-list parsing overhead in the regular interface,
and hoist the lookups outside performance-critical code.  All the
generators are memoised in specials, but accesses are protected by
mutexes (and are atomic on SBCL anyway), so there should not be any
threading issue.

The low-level interface consists of the three following generators;
they don't offer any functionality absent from the easy interface, but
make it possible to dispatch once and subsequently call the right
function directly.

 * `NAPA-FFT:GET-FFT`: 
 * `NAPA-FFT:GET-WINDOWED-FFT`
 * `NAPA-FFT:GET-REVERSE`
 
The previous three generators actually depend on these memoised
generators, which allow even lower-level accesses.

 * `NAPA-FFT:%ENSURE-FFT`
 * `NAPA-FFT:%ENSURE-TWIDDLES`
 * `NAPA-FFT:%ENSURE-REVERSE`

The functions returned by these generators perform virtually no error
checking; make sure to use them correctly.

### GET-FFT

Syntax: `get-fft size &key forward scale in-order => fft-function`.

Arguments and Values:

 * _size_: size of the FFTs to perform (must be a power of two);
 * _forward_: whether the transform is forward (t, default) or inverse
   (nil);
 * _scale_: whether the transform should be unscaled (nil, 1), scaled
   by _1/size_ (t, :inv) or by _1/sqrt(size)_ (sqrt, :sqrt).  Defaults
   to nil for forward transforms and t for inverse ones.
 * _in-order_: whether the transform should be in-order (default, t),
   or have out-of-order output for forward FFTs and out-of-order input
   for inverse FFTs (nil).
 * _fft-function_: a one-argument function that performs an in-place
   transform on its single argument (a complex-sample-array).

`GET-FFT` returns a function that computes the DFT of the first _size_
elements of its single argument.  That argument must be a
complex-sample-array, and will be transformed in-place.

Example:

    CL-USER> (let ((fft (napa-fft:get-fft 8))
                   (data (make-array 8 :element-type 'napa-fft:complex-sample
                                       :initial-element (complex 1d0 0d0))))
               ;; data transformed in-place
               (funcall fft data))
    #(#C(8.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0)
      #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))

### GET-WINDOWED-FFT

Syntax: `get-windowed-fft size window-type &key forward scale in-order => fft-function`

Arguments and Values:

 * _size_: size of the FFTs to perform (must be a power of two);
 * _window-type_: whether the window vector is a simple-array of
   real-sample (float, real-sample) or of complex-sample (complex,
   complex-sample)
 * _forward_: whether the transform is forward (t, default) or inverse
   (nil);
 * _scale_: whether the transform should be unscaled (nil, 1), scaled
   by _1/size_ (t, :inv) or by _1/sqrt(size)_ (sqrt, :sqrt).  Defaults
   to nil for forward transforms and t for inverse ones.
 * _in-order_: whether the transform should be in-order (default, t),
   or have out-of-order output for forward FFTs and out-of-order input
   for inverse FFTs (nil).
 * _function_: a two-argument function that performs an in-place
   transform on its first argument (a complex-sample-array), after
   multiplying it by its second argument (a real-sample-array or a
   complex-sample-array, depending on _window-type_).

`GET-WINDOWED-FFT` returns a function that computes the DFT of the
first _size_ elements of its first argument.  Conceptually, this first
argument is first multiplied element-wise by the second argument (the
window); in practice, this is executed as part of the FFT.  

For inverse FFTs, the multiplication happens after the bit-reversal;
the window should thus itself be bit-reversed.


### GET-REVERSE

Syntax: `get-reverse n &optional eltype => reversal-function`

Arguments and Values:

 * _n_: size of the vector (simple-array) to bit-reverse; must be a
   power of two.
 * _eltype_: element type of the vector, complex-sample (default) or
   real-sample.
 * _reversal-function_: function that perform an in-place bit-reversal
   permutation of the first _n_ elements of its single argument, a
   simple-array with element-type _eltype_.
   
`GET-REVERSE` returns a function that perform an in-place bit-reversal
of the first _size_ elements of its single argument.

### %ENSURE-FFT

Syntax: `%ensure-fft direction scaling windowing n => fft-function`

Arguments and Values:

 * _direction_: whether to perform a forward (1 or :fwd) or inverse (-1,
   :inv or :bwd) transformation.
 * _scaling_: whether to perform an unscaled transform (nil, 1), to
   scale by _1/n_ (t, :inv) or by _1/sqrt(n)_ (sqrt, :sqrt).
 * _windowing_: whether to first perform an element-wise
   multiplication by a real-sample-array (float, real-sample), by a
   complex-sample-array (complex, complex-sample) or no multiplication
   at all (nil).
 * _n_: size of the transform.
 * _fft-function_: if _windowing_ is false, a two-argument function
   that performs an in-place transformation of the _n_ elements of its
   first argument, starting at its second argument.  If _windowing_ is
   true, there are two additional arguments, the window vector and the
   start index. Finally, the last argument is an appropriately-sized
   vector of twiddle factors.

`%ENSURE-FFT` is similar to `GET-FFT` or `GET-WINDOWED-FFT`, except
that the start indices may be specified.

### %ENSURE-TWIDDLES

Syntax: `%ensure-twiddles n forwardp => twiddle-vector`

Argument and Values:

 * _n_: size of the transform to get a twiddle vector for; must be a
   power of two.
 * _forwardp_: whether the transform is a forward one (t) or inverse
   (nil).
 * _twiddle-vector_: complex-sample-array of twiddle factors for a
   split-radix Cooley-Tukey FFT of size at least _n_.

`%ENSURE-TWIDDLES` computes and caches vectors that should be passed
as the last argument of the function returned by `%ENSURE-FFT`.  The
structure of the vectors is such that a vector appropriate for a
transform of size _n_ is also appropriate for all smaller sizes (with
the same direction).

### %ENSURE-REVERSE

Syntax `%ensure-reverse n &optional eltype => reversal-function`

Argument and Values:

 * _n_: size of the vector (simple-array) to bit-reverse; must be a
   power of two.
 * _eltype_: element type of the vector, complex-sample (default) or
   real-sample.
 * _reversal-function_: function that perform an in-place bit-reversal
   permutation of _n_ elements of its single argument, a simple-array
   with element-type _eltype_, starting at the index given by its
   second argument.

`%ENSURE-REVERSE` is exactly like `GET-REVERSE`, but allows the user
to specify the starting index of the vector to bit-reverse.

Implementation
--------------

Napa-FFT3 is based on a split-radix, out-of-order and in-place variant
of the Cooley-Tukey FFT algorithm.  Split-radix is relatively simple,
and achieve operation counts very close (within a couple percents) to
the minimum known so far.  Doing it out of order simplifies the
transform code a lot; this way, all the accesses are naturally
in-place and follow a streaming order, at each level of the recursion.
Better: by executing the recursion depth-first rather than
breadth-first, the code exploits caches implicitly.  Finally, an
in-place transform can hope to fit nearly twice as large inputs in
cache as an out-of-place one, as there is no auxiliary output vector.
All in all, it looks like a good choice of algorithm: close enough to
the theoretical optimum, interesting performance properties, and not
too complex to implement.

Many operations are as easily expressed on bit-reversed as on
natural-order frequency-domain values (e.g. convolutions, or filtering
noise out).  That's not always the case, unfortunately, so Napa-FFT
also implements a bit-reversal pass.

In the past, this was often slow enough to make it vastly preferable
to instead implement an in-order (autosorting) FFT: the slow,
bandwidth-bound, bit-reversal is merged with the more arithmetic-heavy
FFT, hopefully resulting in faster code than executing each serially.
However, as [Karp] points out, this seems to be better explained by
bad code than anything else.  [Karter and Gatlin] build on that and
describe an algorithm designed to exploit memory caches, ensuring at
most two misses per cache line of data; this is enough to obtain much
better performance (or comparable) than all the algorithms reviewed in
[Karp] across a range of nearly-contemporary machines.  In a later
paper, [Zhang and Zhang], note that we can exploit the high
associativity in certain caches to simplify the code a lot, or improve
on its performance.  Their algorithms are somewhat complicated by
explicit blocking loops; a clever recursion suffices to obtain access
patterns appropriate for nearly all block sizes.

Split-radix FFT
===============

The split-radix algorithm has a very short recursive definition;
practically however, we want larger base cases than the strict
minimum.  Rather than depending on hand-written specialised base
cases, Napa-FFT3 includes a specialised compiler that turns the
execution trace for a given FFT into straight-line code.  This way, we
maintain the near-optimal operation count, especially since the code
to multiply by twiddle factors can be optimised for "nice" constants
(e.g. 1, i or sqrt(i)).  The specialised compiler takes care of
spilling values to the data vector according to Belady's algorithm.
This will tend to perform a lot better than general-purpose spilling
logic (e.g. SBCL's coloring-based algorithm), and spilled values are
stored at correctly-aligned addresses, following a cache-friendly
layout.

This, along with specialised recursive steps for each size, gives us
high-performance out-of-order transforms.  Much of the complexity in
FFTs comes from ensuring the data are in natural order; it's not
surprising that simple code can achieve runtimes comparable or lower
than sophisticated code like FFTW once that constraint is relaxed.

The base cases are nevertheless incredibly naive, compared to FFTW's
codelets.  For tiny transforms, Napa-FFT is clearly not in the same
league; however, as cache effects gain importance, the higher-level
design choices pay off, and out-of-order Napa-FFT closes the gap with
FFTW.  In fact, it is even slightly faster for very large transforms.

Bit reversal
============

More surprising might be the fact that quick bit-reversals are now so
easy to design.  Much of the runtime in Napa-FFT2 was caused by the
transposition steps.  At first sight, a bit-reversal is even more
complicated.  However, bit-reversals have the nice property that they
only consist of swaps: they can be easily be executed in-place, by
swapping each element with its destination.  In contrast, this is only
true for transpose of square matrices (FFT sizes that are even powers
of two).

Historically, on cache-ful computers, the issue with bit-reversal has
been one of aliasing in low-associativity caches: the swap pattern
involves addresses that tend to be mapped to the same cache lines.
The workarounds involve some sort of software buffering, either in
registers or in an auxiliary array, to supplement the caches.

Nowadays, however, computer architects have more transistors than they
know what to do with, so caches are large and have high associativity
(at least 4-way at the L2 or L3 level on both AMD and Intel chips).
Since we're mostly conerned with bit-reversing complex-sample-vectors,
each element is a pair of double-floats, and only 4 fit in each cache
line.  In this case, as [Zhang and Zhang 99] point out, the
associativity is high enough not to necessitate any software
buffering!

The novel part seems to be the traversal order.  [Zhang and Zhang 99]
point out that blocking helps with locality, and a few older work
[Elster, Rutkowska] have described how bit-reversal swaps could be
generated recursively.  The blocking described in [Zhang and Zhang 99]
is very much cache-aware, and must be explicitly structured to take
advantage of cache levels and TLBs.  It seems that all the
recursively-generated swaps generate indices to swap from the outside
in; that is, the leaf of the recursion have all the bits fixed except
for the middle ones.  If we wish to enhance locality, we should fix
the middle bit first, and, at the leaves, have the outermost
(i.e. most significant, but also least significant) bits vary.  This
way, we implicitly get blocking for any cache line size (given
sufficient associativity), but also for fully-associative TLBs.

Another way to see this is that each swap tends to be between vastly
different indices (bit reversal is bad for locality).  One classic way
to deal with this is to sort the swaps in Z-order, by interleaving
bits from each swapped index.  Unfortunately, with bit reversal, this
is equivalent to recursing from the outside.  Instead, we can sort the
least significant half of the indices in Z-order, and that will make
the outermost bit vary between adjacent swaps.

Obviously, determining this ordering at runtime is a lot of work.
Instead, specialised leaf routines that handle changing, e.g., the top
and bottom -most three bits (in the right traversal order), are called
with the middle bits found in a pre-sorted vector.

On my workstation, this results in .3 cache miss/element (for complex
double floats), which is only a bit more than the compulsory .25
miss/element.  Surprisingly, while there are a lot ofTLB misses (at
the first level, I suppose), eliminating them by switching to huge
pages doesn't really improve runtimes.  I'm currently thinking that's
because the traversal order is actually tuned for the last level TLB,
which is fully-associative.  In the end, the net effect is that bit
reversal of large vectors hits around 60 % of my workstation's
out-of-cache *streaming* bandwidth (as measured by STREAM's copy
loop).  It's not instantaneous, but not an insurmountable handicap
either.

