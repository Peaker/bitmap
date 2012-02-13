
/*
the C part of the Data.Bitmap library
(c) 2009 Balazs Komuves
license: BSD3
*/

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "bm.h"

// -----------------------------------------------------------------------------

#if __STDC_VERSION__ >= 199901L
/* "inline" is a keyword */
#else
#define inline /* nothing */
#endif

// -----------------------------------------------------------------------------
 
// (to, count, fill) !!! 
void c_memset(word8 *q, int count, word8 x)
{ memset(q,x,count);
}

// (from, to, count) !!!
void c_memcpy(word8 *p, word8 *q, int count)
{ memcpy(q,p,count);
}

// -----------------------------------------------------------------------------

#define K_WORD8  1
#define K_WORD16 2
#define K_WORD32 3
#define K_FLOAT  4

#define PLUSPTR(P,TYP,K) (TYP*)(((word8*)(P))+(K));

#define MAX(A,B) (((A)>(B))?(A):(B))
#define MIN(A,B) (((A)<(B))?(A):(B))
#define CLAMP(X,A,B) MIN(MAX(X,A),B)
#define CLAMP_01(X) MIN(MAX(X,0),1)

// -----------------------------------------------------------------------------

inline float toFloat_word8  (word8  x) { return ( (float)x * (3.92156862745098e-3   ) ); }
inline float toFloat_word16 (word16 x) { return ( (float)x * (1.5259021896696422e-5 ) ); }
inline float toFloat_word32 (word32 x) { return ( (float)x * (2.3283064370807974e-10) ); }
inline float toFloat_float  (float  x) { return x; }

inline word8  fromFloat_word8  (float x) { return ( floor ( 0.5 + 255.0        * CLAMP_01(x) ) ); }
inline word16 fromFloat_word16 (float x) { return ( floor ( 0.5 + 65535.0      * CLAMP_01(x) ) ); }
inline word32 fromFloat_word32 (float x) { return ( floor ( 0.5 + 4294967295.0 * CLAMP_01(x) ) ); }
inline float  fromFloat_float  (float x) { return x; }

// -----------------------------------------------------------------------------

#define C_EXTRACT_CHANNEL(TYP) \
void c_extract_channel_ ## TYP \
  ( int width, int height \
  , TYP *p1, int nchn1, int pad1, int ofs1 \
  , TYP *p2, int nchn2, int pad2, int ofs2 \
  ) \
{ int x,y;        \
  TYP *q1,*q2;    \
  q1 = p1 + ofs1; \
  q2 = p2 + ofs2; \
  for(y=0;y<height;y++)   \
  { for(x=0;x<width;x++)  \
    { *q2 = *q1;    \
      q1 += nchn1;  \
      q2 += nchn2;  \
    }               \
    q1 = PLUSPTR(q1,TYP,pad1); \
    q2 = PLUSPTR(q2,TYP,pad2); \
  }   \
} 

C_EXTRACT_CHANNEL(word8 )
C_EXTRACT_CHANNEL(word16)
C_EXTRACT_CHANNEL(word32)
C_EXTRACT_CHANNEL(float )

#define CALL_EXTRACT_CHANNEL(TYP) \
c_extract_channel_ ## TYP ( width,height, p1,nchn1,pad1,ofs1, p2,nchn2,pad2,ofs2 );

// offset is measured in components, not bytes!
void c_extract_channel
  ( int k_type
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  ) 
{ switch(k_type)
  { case K_WORD8:  CALL_EXTRACT_CHANNEL(word8 ); break;
    case K_WORD16: CALL_EXTRACT_CHANNEL(word16); break;
    case K_WORD32: CALL_EXTRACT_CHANNEL(word32); break;
    case K_FLOAT:  CALL_EXTRACT_CHANNEL(float ); break;
  }
}

// -----------------------------------------------------------------------------

// hmm hmm nem jo meg, tullogunk a sorok vegen meg alul is :(

// it's a little bit hackish, but i think that's ok 
void interpolate_hack(int width1,int width2, int x, float *fx, int *ix)
{ float t,f;
  int j;
  t = ((float)x+0.5) * ((float)width1) / ((float)width2) - 0.5;  
  j = floor(t);
  f = t-j;
  if ( j < 0 )
  { *ix = 0;
    *fx = 0.0;
  }
  else
  { if ( j > (width1-2) )
    { *ix = width1-2;
      *fx = 1.0;
    }
    else
    { *ix = j;
      *fx = f;
    }
  }
}

// should we ask haskell for allocating the memory for the tables instead of malloc?
#define C_BILINEAR_RESAMPLE_CHANNEL(TYP) \
void c_bilinear_resample_channel_ ## TYP  \
  ( int width1, int height1, TYP *p1, int nchn1, int pad1, int ofs1 \
  , int width2, int height2, TYP *p2, int nchn2, int pad2, int ofs2 \
  )                        \
{ int x,y;                 \
  TYP *q2,*s1,*s2,*r1,*r2; \
  float fx,fy;             \
  int ix,iy;               \
  int linesize1;           \
  float *fhoriztable;      \
  int   *ihoriztable;      \
  float a,b,c;             \
                  \
  q2 = p2 + ofs2; \
  linesize1 = nchn1 * width1 * sizeof(TYP) + pad1; \
                                                   \
  fhoriztable = malloc(sizeof(float)*width2);      \
  ihoriztable = malloc(sizeof(int  )*width2);      \
  for(x=0;x<width2;x++)    \
  { interpolate_hack( width1,width2, x, fhoriztable+x, ihoriztable+x); \
    ihoriztable[x] *= sizeof(TYP) * nchn1; /* !!! */                   \
  }                        \
                           \
  for(y=0;y<height2;y++)   \
  { interpolate_hack( height1,height2, y, &fy, &iy );   \
    s1 = PLUSPTR ( p1 + ofs1 , TYP , iy * linesize1 );  \
    s2 = PLUSPTR ( s1        , TYP ,      linesize1 );  \
    for(x=0;x<width2;x++)      \
    { fx = fhoriztable[x];     \
      ix = ihoriztable[x];     \
      r1 = PLUSPTR ( s1, TYP, ix ); /* we multiplied up during precalc! */ \
      r2 = PLUSPTR ( s2, TYP, ix ); \
      a = (1-fx) * (float)(r1[0]) + fx * (float)(r1[nchn1]); \
      b = (1-fx) * (float)(r2[0]) + fx * (float)(r2[nchn1]); \
      c = (1-fy)*a + fy*b;   \
      *q2 = (TYP)c;   /* round? but what about the floats? */ \
      q2 += nchn2;        \
    }                     \
    q2 = PLUSPTR(q2,TYP,pad2); \
  }                  \
  free(ihoriztable); \
  free(fhoriztable); \
} 

C_BILINEAR_RESAMPLE_CHANNEL(word8 )
C_BILINEAR_RESAMPLE_CHANNEL(word16)
C_BILINEAR_RESAMPLE_CHANNEL(word32)
C_BILINEAR_RESAMPLE_CHANNEL(float )

#define CALL_BILINEAR_RESAMPLE_CHANNEL(TYP) \
c_bilinear_resample_channel_ ## TYP ( width1,height1,p1,nchn1,pad1,ofs1, width2,height2,p2,nchn2,pad2,ofs2 );

// offset is measured in components, not bytes!
void c_bilinear_resample_channel
  ( int k_type
  , int width1, int height1, void *p1, int nchn1, int pad1, int ofs1 
  , int width2, int height2, void *p2, int nchn2, int pad2, int ofs2 
  ) 
{ switch(k_type)
  { case K_WORD8:  CALL_BILINEAR_RESAMPLE_CHANNEL(word8 ); break; 
    case K_WORD16: CALL_BILINEAR_RESAMPLE_CHANNEL(word16); break;  
    case K_WORD32: CALL_BILINEAR_RESAMPLE_CHANNEL(word32); break; 
    case K_FLOAT:  CALL_BILINEAR_RESAMPLE_CHANNEL(float ); break; 
  }
}

// -----------------------------------------------------------------------------

#define GAMMA_CORRECT_COMPONENT(TYP,value,gamma) \
  fromFloat_ ## TYP ( exp ( (gamma) * log ( 0.0000001 + toFloat_ ## TYP ( (value) ) ) ) )

#define C_GAMMA_CORRECT_CHANNEL(TYP) \
void c_gamma_correct_channel_ ## TYP \
  ( float gamma           \
  , int width, int height \
  , TYP *p1, int nchn1, int pad1, int ofs1 \
  , TYP *p2, int nchn2, int pad2, int ofs2 \
  ) \
{ int x,y;        \
  TYP *q1,*q2;    \
  q1 = p1 + ofs1; \
  q2 = p2 + ofs2; \
  for(y=0;y<height;y++)   \
  { for(x=0;x<width;x++)  \
    { *q2 = GAMMA_CORRECT_COMPONENT (TYP , *q1 , gamma);    \
      q1 += nchn1;  \
      q2 += nchn2;  \
    }               \
    q1 = PLUSPTR(q1,TYP,pad1); \
    q2 = PLUSPTR(q2,TYP,pad2); \
  }   \
} 

C_GAMMA_CORRECT_CHANNEL(word8 )
C_GAMMA_CORRECT_CHANNEL(word16)
C_GAMMA_CORRECT_CHANNEL(word32)
C_GAMMA_CORRECT_CHANNEL(float )

#define C_GAMMA_CORRECT_ALL_CHANNELS(TYP) \
void c_gamma_correct_all_channels_ ## TYP \
  ( float gamma                     \
  , int width, int height, int nchn \
  , TYP *p1, int pad1 \
  , TYP *p2, int pad2 \
  ) \
{ int x,y;          \
  int compsPerLine; \
  TYP *q1,*q2;      \
  q1 = p1;   \
  q2 = p2;   \
  compsPerLine = width * nchn; \
  for(y=0;y<height;y++)   \
  { for(x=0;x<compsPerLine;x++)  \
    { *q2 = GAMMA_CORRECT_COMPONENT (TYP , *q1 , gamma);    \
      q1 ++ ;  \
      q2 ++ ;  \
    }               \
    q1 = PLUSPTR(q1,TYP,pad1); \
    q2 = PLUSPTR(q2,TYP,pad2); \
  }   \
} 

C_GAMMA_CORRECT_ALL_CHANNELS(word8 )
C_GAMMA_CORRECT_ALL_CHANNELS(word16)
C_GAMMA_CORRECT_ALL_CHANNELS(word32)
C_GAMMA_CORRECT_ALL_CHANNELS(float )

#define CALL_GAMMA_CORRECT_CHANNEL(TYP) \
c_gamma_correct_channel_ ## TYP ( gamma, width,height, p1,nchn1,pad1,ofs1, p2,nchn2,pad2,ofs2 );

// offset is measured in components, not bytes!
void c_gamma_correct_channel
  ( int k_type
  , float gamma
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  ) 
{ switch(k_type)
  { case K_WORD8:  CALL_GAMMA_CORRECT_CHANNEL(word8 ); break; 
    case K_WORD16: CALL_GAMMA_CORRECT_CHANNEL(word16); break;  
    case K_WORD32: CALL_GAMMA_CORRECT_CHANNEL(word32); break; 
    case K_FLOAT:  CALL_GAMMA_CORRECT_CHANNEL(float ); break; 
  }
}

#define CALL_GAMMA_CORRECT_ALL_CHANNELS(TYP) \
c_gamma_correct_all_channels_ ## TYP ( gamma, width,height, nchn, p1,pad1, p2,pad2 );

// offset is measured in components, not bytes!
void c_gamma_correct_all_channels
  ( int k_type
  , float gamma
  , int width, int height, int nchn
  , void *p1, int pad1 
  , void *p2, int pad2 
  ) 
{ switch(k_type)
  { case K_WORD8:  CALL_GAMMA_CORRECT_ALL_CHANNELS(word8 ); break; 
    case K_WORD16: CALL_GAMMA_CORRECT_ALL_CHANNELS(word16); break;  
    case K_WORD32: CALL_GAMMA_CORRECT_ALL_CHANNELS(word32); break; 
    case K_FLOAT:  CALL_GAMMA_CORRECT_ALL_CHANNELS(float ); break; 
  }
}

// -----------------------------------------------------------------------------


#define LINEAR_COMBINE_COMPONENT(TYP,value1,value2,weight1,weight2) \
  fromFloat_ ## TYP ( weight1 * toFloat_ ## TYP (value1) +  weight2 * toFloat_ ## TYP (value2) ) 
  
#define C_LINEAR_COMBINE_CHANNELS(TYP) \
void c_linear_combine_channels_ ## TYP \
  ( float weight1, float weight2       \
  , int width, int height \
  , TYP *p1, int nchn1, int pad1, int ofs1 \
  , TYP *p2, int nchn2, int pad2, int ofs2 \
  , TYP *p3, int nchn3, int pad3, int ofs3 \
  ) \
{ int x,y;         \
  TYP *q1,*q2,*q3; \
  q1 = p1 + ofs1;  \
  q2 = p2 + ofs2;  \
  q3 = p3 + ofs3;  \
  for(y=0;y<height;y++)   \
  { for(x=0;x<width;x++)  \
    { *q3 = LINEAR_COMBINE_COMPONENT (TYP , *q1 , *q2 , weight1 , weight2);    \
      q1 += nchn1;  \
      q2 += nchn2;  \
      q3 += nchn3;  \
    }               \
    q1 = PLUSPTR(q1,TYP,pad1); \
    q2 = PLUSPTR(q2,TYP,pad2); \
    q3 = PLUSPTR(q3,TYP,pad3); \
  }   \
} 

C_LINEAR_COMBINE_CHANNELS(word8 )
C_LINEAR_COMBINE_CHANNELS(word16)
C_LINEAR_COMBINE_CHANNELS(word32)
C_LINEAR_COMBINE_CHANNELS(float )

#define CALL_LINEAR_COMBINE_CHANNELS(TYP) \
c_linear_combine_channels_ ## TYP         \
  ( weight1,weight2, width,height,        \
    p1,nchn1,pad1,ofs1,      \
    p2,nchn2,pad2,ofs2,      \
    p3,nchn3,pad3,ofs3 );

// offset is measured in components, not bytes!
void c_linear_combine_channels
  ( int k_type
  , float weight1, float weight2
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  , void *p3, int nchn3, int pad3, int ofs3 
  ) 
{ switch(k_type)
  { case K_WORD8:  CALL_LINEAR_COMBINE_CHANNELS(word8 ); break; 
    case K_WORD16: CALL_LINEAR_COMBINE_CHANNELS(word16); break;  
    case K_WORD32: CALL_LINEAR_COMBINE_CHANNELS(word32); break; 
    case K_FLOAT:  CALL_LINEAR_COMBINE_CHANNELS(float ); break; 
  }
}

// -----------------------------------------------------------------------------


