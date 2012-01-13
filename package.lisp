(defpackage "NAPA-FFT"
  (:use)
  (:export "COMPLEX-SAMPLE" "COMPLEX-SAMPLE-ARRAY"
           "DIRECTION" "SCALING" "WINDOWING"
           "%ENSURE-TWIDDLES"
           "%ENSURE-REVERSE" "GET-REVERSE"
           "%ENSURE-FFT" "GET-FFT" "GET-WINDOWED-FFT"

           "*SCRATCH*" "*DOUBLE-SCRATCH*"
           "BIT-REVERSE"
           "FFT" "IFFT"

           "RECTANGULAR" "HANN" "BLACKMAN*"
           "BLACKMAN" "TRIANGLE" "BARTLETT"
           "GAUSS*" "GAUSSIAN" "GAUSSIAN*BARTLETT^X"
           "COSINE-SERIES" "BLACKMAN-HARRIS"

           "WINDOW-VECTOR"
           "CLIP-IN-WINDOW"
           "EXTRACT-WINDOW-INTO" "EXTRACT-WINDOW"
           "EXTRACT-CENTERED-WINDOW-INTO"
           "EXTRACT-CENTERED-WINDOW"

           "WINDOWED-FFT"))

(defpackage "NAPA-FFT.SUPPORT"
  (:use "CL" "SB-EXT" "NAPA-FFT")
  (:export "INDEX" "HALF-INDEX"
           "SIZE" "HALF-SIZE"
           "COMPLEX-SAMPLE" "COMPLEX-SAMPLE-ARRAY"
           "BIT-REVERSE-INTEGER"
           "MAKE-TWIDDLE" "+TWIDDLE-OFFSET+"
           "LB" "POWER-OF-TWO-P"))

(defpackage "NAPA-FFT.GEN"
  (:use "CL" "SB-EXT" "NAPA-FFT" "NAPA-FFT.SUPPORT")
  (:export "GEN-DIF" "GEN-DIT" "GEN-BIT-REVERSAL"
           "VEC" "START" "TMP" "STARTT" "TWIDDLE"
           "WINDOW" "WINDOW-START"))

(defpackage "NAPA-FFT.IMPL"
  (:use "CL" "SB-EXT" "NAPA-FFT"
        "NAPA-FFT.SUPPORT"
        "NAPA-FFT.GEN"))

(defpackage "NAPA-FFT.TESTS"
  (:use "CL" "SB-EXT"
        "NAPA-FFT"
        "NAPA-FFT.SUPPORT"
        "NAPA-FFT.GEN")
  (:export "*FANCY-IN-ORDER*"
           "MAKE-SCALED-FWD"
           "MAKE-SCALED-INV"
           "GET-FANCY-FWD"
           "GET-FANCY-INV"

           "MAKE-WINDOWED-FWD"
           "MAKE-WINDOWED-INV"
           "GET-FANCY-WINDOWED-FWD"
           "GET-FANCY-WINDOWED-INV"
           
           "FORWARD-TEST" "RUN-FORWARD-TESTS"
           "TEST-PAIRS"
           "RUN-PAIRS"
           
           "TEST-WINDOW"
           "RUN-WINDOWS"

           "TEST-OFFSET"
           "RUN-OFFSETS"))
