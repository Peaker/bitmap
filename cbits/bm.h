
#include <stdint.h>

typedef uint8_t  word8;
typedef uint16_t word16;
typedef uint32_t word32;

//------------------------------------------------------------------------------

// (target, count, fill)
void c_memset(word8 *q, int count, word8 x);

// (from, to, count)
void c_memcpy(word8 *p, word8 *q, int count);

//------------------------------------------------------------------------------

void c_extract_channel
  ( int k_type
  , int width, int height 
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );

//------------------------------------------------------------------------------

// offset is measured in components, not bytes!
void c_bilinear_resample_channel
  ( int k_type
  , int width1, int height1, void *p1, int nchn1, int pad1, int ofs1 
  , int width2, int height2, void *p2, int nchn2, int pad2, int ofs2 
  );

//------------------------------------------------------------------------------

void c_gamma_correct_channel
  ( int k_type
  , float gamma
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  );
  
void c_gamma_correct_all_channels
  ( int k_type
  , float gamma
  , int width, int height, int nchn
  , void *p1, int pad1 
  , void *p2, int pad2 
  );

//------------------------------------------------------------------------------

// offset is measured in components, not bytes!
void c_linear_combine_channels
  ( int k_type
  , float weight1, float weight2
  , int width, int height
  , void *p1, int nchn1, int pad1, int ofs1 
  , void *p2, int nchn2, int pad2, int ofs2 
  , void *p3, int nchn3, int pad3, int ofs3 
  );
  
//------------------------------------------------------------------------------
      