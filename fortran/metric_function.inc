      ! metric_function.inc (overwritten each run - lll(i,j,x) entries)
      if (i.eq.1) then
       if (j.eq.1) then
        lll=1.000000000000000d0
       else if (j.eq.2) then
        lll=0.000000000000000d0
       endif
      else if (i.eq.2) then
       if (j.eq.1) then
        lll=0.000000000000000d0
       else if (j.eq.2) then
        lll=1.000000000000000d0
       endif
      endif

