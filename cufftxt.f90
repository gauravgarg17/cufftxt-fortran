! CUFFTXt Fortran interface

module cufftxt

  use cudafor
  use cufft
  use cudalibxt

  implicit none

  enum, bind(C)
     enumerator :: CUFFT_XT_FORMAT_INPUT              = 0
     enumerator :: CUFFT_XT_FORMAT_OUTPUT             = 1
     enumerator :: CUFFT_XT_FORMAT_INPLACE            = 2
     enumerator :: CUFFT_XT_FORMAT_INPLACE_SHUFFLED   = 3
     enumerator :: CUFFT_XT_FORMAT_1D_INPUT_SHUFFLED  = 4
     enumerator :: CUFFT_FORMAT_UNDEFINED             = 5
  end enum

  enum, bind(C)
     enumerator :: CUFFT_COPY_HOST_TO_DEVICE    = 0
     enumerator :: CUFFT_COPY_DEVICE_TO_HOST    = 1
     enumerator :: CUFFT_COPY_DEVICE_TO_DEVICE  = 2
     enumerator :: CUFFT_COPY_UNDEFINED         = 3
  end enum

  enum, bind(C)
     enumerator :: CUFFT_QUERY_1D_FACTORS   = 0
     enumerator :: CUFFT_QUERY_UNDEFINED    = 1
  end enum

  type, bind(C) :: cufftXt1dFactors
    integer(c_long_long) :: size;
    integer(c_long_long) :: stringCount;
    integer(c_long_long) :: stringLength;
    integer(c_long_long) :: substringLength;
    integer(c_long_long) :: factor1;
    integer(c_long_long) :: factor2;
    integer(c_long_long) :: stringMask;
    integer(c_long_long) :: substringMask;
    integer(c_long_long) :: factor1Mask;
    integer(c_long_long) :: factor2Mask;
    integer(c_long_long) :: stringShift;
    integer(c_long_long) :: substringShift;
    integer(c_long_long) :: factor1Shift;
    integer(c_long_long) :: factor2Shift;
  end type cufftXt1dFactors

  enum, bind(C)
     enumerator :: CUFFT_WORKAREA_MINIMAL       = 0
     enumerator :: CUFFT_WORKAREA_USER          = 1
     enumerator :: CUFFT_WORKAREA_PERFORMANCE   = 2
  end enum

! cufftXtSetGPUs
  interface cufftXtSetGPUs
     integer function cufftXtSetGPUs(handle, nGPUs, whichGPUs) bind(C,name='cufftXtSetGPUs')
       integer, value :: handle
       integer, value :: nGPUs
       integer, dimension(:) :: whichGPUs
     end function cufftXtSetGPUs
  end interface cufftXtSetGPUs
  
! cufftXtMalloc
  interface cufftXtMalloc
     integer function cufftXtMalloc( plan, descriptor, format) bind(C,name='cufftXtMalloc')
     import :: cudaLibXtDesc
       integer, value :: plan
       type(cudaLibXtDesc), pointer, intent(out) :: descriptor
       integer, value :: format
     end function cufftXtMalloc
  end interface cufftXtMalloc
  
! cufftXtMemcpy
  interface cufftXtMemcpy
     integer function cufftXtMemcpyH2D( plan, dstPointer, srcPointer, type) bind(C,name='cufftXtMemcpy')
     import :: cudaLibXtDesc
       integer, value :: plan
       type(cudaLibXtDesc) :: dstPointer
       real(kind(1.d0)), dimension(*) :: srcPointer
       integer, value :: type
     end function cufftXtMemcpyH2D
      
     integer function cufftXtMemcpyD2H( plan, dstPointer, srcPointer, type) bind(C,name='cufftXtMemcpy')
      import :: cudaLibXtDesc
       integer, value :: plan
       real(kind(1.d0)), dimension(*) :: dstPointer
       type(cudaLibXtDesc) :: srcPointer
       integer, value :: type
      end function cufftXtMemcpyD2H
  end interface cufftXtMemcpy

! cufftXtFree
  interface cufftXtFree
     integer function cufftXtFree(descriptor) bind(C,name='cufftXtFree')
     import :: cudaLibXtDesc
       type(cudaLibXtDesc) :: descriptor
     end function cufftXtFree
  end interface cufftXtFree

  ! cufftMakePlan3d
  interface cufftMakePlan3d
     integer(4) function cufftMakePlan3d(plan, nx, ny, nz, ffttype, workSize) bind(C,name='cufftMakePlan3d')
       integer(4), value :: plan
       integer(4), value :: nx, ny, nz
       integer(4), value :: ffttype
       integer(kind=int_ptr_kind()), dimension(:) :: workSize
     end function cufftMakePlan3d
  end interface cufftMakePlan3d

! cufftCreate
  interface cufftXtSetWorkArea
     integer(4) function cufftXtSetWorkArea(plan, workArea) bind(C, name='cufftXtSetWorkArea')
     import :: c_devptr
       integer(4), value :: plan
       type(c_devptr), dimension(:) :: workArea !! double pointer in C interface
     end function cufftXtSetWorkArea
  end interface cufftXtSetWorkArea

! cufftXtExecDescriptorC2C
  interface cufftXtExecDescriptorC2C
     integer(4) function cufftXtExecDescriptorC2C(plan, input, output, direction) bind(C,name='cufftXtExecDescriptorC2C')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
       integer(4), value :: direction
     end function cufftXtExecDescriptorC2C
  end interface cufftXtExecDescriptorC2C
  
  ! cufftXtExecDescriptorC2C
  interface cufftXtExecDescriptorR2C
     integer(4) function cufftXtExecDescriptorR2C(plan, input, output) bind(C,name='cufftXtExecDescriptorR2C')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
     end function cufftXtExecDescriptorR2C
  end interface cufftXtExecDescriptorR2C

  ! cufftXtExecDescriptorC2R
  interface cufftXtExecDescriptorC2R
     integer(4) function cufftXtExecDescriptorC2R(plan, input, output) bind(C,name='cufftXtExecDescriptorC2R')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
     end function cufftXtExecDescriptorC2R
  end interface cufftXtExecDescriptorC2R

  ! cufftXtExecDescriptorC2C
  interface cufftXtExecDescriptorZ2Z
     integer(4) function cufftXtExecDescriptorZ2Z(plan, input, output, direction) bind(C,name='cufftXtExecDescriptorZ2Z')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
       integer(4), value :: direction
     end function cufftXtExecDescriptorZ2Z
  end interface cufftXtExecDescriptorZ2Z

  ! cufftXtExecDescriptorD2Z
  interface cufftXtExecDescriptorD2Z
     integer(4) function cufftXtExecDescriptorD2Z(plan, input, output) bind(C,name='cufftXtExecDescriptorD2Z')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
     end function cufftXtExecDescriptorD2Z
  end interface cufftXtExecDescriptorD2Z

  ! cufftXtExecDescriptorZ2D
  interface cufftXtExecDescriptorZ2D
     integer(4) function cufftXtExecDescriptorZ2D(plan, input, output) bind(C,name='cufftXtExecDescriptorZ2D')
     import :: cudaLibXtDesc
       integer(4), value :: plan
       type(cudaLibXtDesc) :: input, output
     end function cufftXtExecDescriptorZ2D
  end interface cufftXtExecDescriptorZ2D
  
end module cufftxt
