! 
!     Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
!
! NVIDIA CORPORATION and its licensors retain all intellectual property
! and proprietary rights in and to this software, related documentation
! and any modifications thereto.  Any use, reproduction, disclosure or
! distribution of this software and related documentation without an express
! license agreement from NVIDIA CORPORATION is strictly prohibited.
! 

! cudalibxt Fortran interface

module cudalibxt

  use iso_c_binding
  use cudafor

  implicit none

  enum, bind(C)
     enumerator :: LIB_XT_COPY_HOST_TO_DEVICE     = 0
     enumerator :: LIB_XT_COPY_DEVICE_TO_HOST     = 1
     enumerator :: LIB_XT_COPY_DEVICE_TO_DEVICE   = 2
  end enum

  enum, bind(C)
     enumerator :: LIB_FORMAT_CUFFT        = 0
     enumerator :: LIB_FORMAT_UNDEFINED    = 1
  end enum

  integer, parameter :: MAX_CUDA_DESCRIPTOR_GPUS = 64

  type, bind(C) :: cudaXtDesc
    integer(c_int) :: version
    integer(c_int) :: nGPUs
    integer(c_int) :: GPUs(MAX_CUDA_DESCRIPTOR_GPUS)
    type(c_devptr) :: data(MAX_CUDA_DESCRIPTOR_GPUS)
    integer(c_intptr_t) :: size(MAX_CUDA_DESCRIPTOR_GPUS)
    type(c_ptr) :: cudaXtState
  end type cudaXtDesc

  type, bind(C) :: cudaLibXtDesc
    integer(c_int) :: version
    type(cudaXtDesc), pointer :: descriptor
    integer(c_int) :: library
    integer(c_int) :: subFormat
    type(c_ptr) :: libDescriptor
  end type cudaLibXtDesc

end module cudalibxt
