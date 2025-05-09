-- This file is part of VulkAda.

-- VulkAda is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- VulkAda is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with VulkAda.
-- If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2025 Phaser Cat Games LLC

-- Basic Vulkan types and constants

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with System;
with Interfaces.C.Extensions;

package Vulkan is
    use type Interfaces.C.unsigned;

    -- Various numeric types.
    type Width is new Interfaces.Unsigned_32;
    type Height is new Interfaces.Unsigned_32;
    type Depth is new Interfaces.Unsigned_32;
    type X_Coordinate is new Interfaces.Integer_32;
    type Y_Coordinate is new Interfaces.Integer_32;
    type Z_Coordinate is new Interfaces.Integer_32;
    type Mip_Levels is new Interfaces.Unsigned_32;
    type Array_Layers is new Interfaces.Unsigned_32;
    type Device_Size is new Interfaces.Unsigned_64;
    type Sample_Mask is new Interfaces.Unsigned_32;
    type UUID is array (1 .. 16) of aliased Interfaces.Unsigned_8
        with Convention => C;
    type LUID is array (1 .. 8) of aliased Interfaces.Unsigned_8
        with Convention => C;
    type Flags is new Interfaces.Unsigned_32;
    type Flags_64 is new Interfaces.Unsigned_64;
    type Queue_Family_Index is new Interfaces.Unsigned_32;
    type Semaphore_Value is new Interfaces.Unsigned_64;
    type Device_Address is new Interfaces.Unsigned_64;
    type Packed_Bit is range 0 .. 1;
    type File_Descriptor is new Interfaces.C.int;

    package Device_Size_Vectors is
        new Ada.Containers.Vectors(Positive, Device_Size);

    -- Constants.
    Attachment_Unused: constant := Interfaces.C.unsigned'Last;
    Lod_Clamp_None: constant := 1000.0;
    Queue_Family_Ignored: constant := Interfaces.C.unsigned'Last;
    Remaining_Array_Layers: constant := Interfaces.C.unsigned'Last;
    Remaining_Mip_Levels: constant := Interfaces.C.unsigned'Last;
    Subpass_External: constant := Interfaces.C.unsigned'Last;
    Whole_Size: constant Device_Size := Device_Size'Last;
    Max_Memory_Types: constant := 32;
    Max_Physical_Device_Name_Size: constant := 256;
    UUID_Size: constant := UUID'Length;
    Max_Extension_Name_Size: constant := 256;
    Max_Description_Size: constant := 256;
    Max_Memory_Heaps: constant := 16;
    No_Timeout: constant := Interfaces.C.unsigned_long_long'Last; 
    -- Vulkan 1.1
    Max_Device_Group_Size: constant := 32;
    Queue_Family_External: constant := Interfaces.C.unsigned'Last - 1;
    -- Vulkan 1.2
    Max_Driver_Name_Size: constant := 256;
    Max_Driver_Info_Size: constant := 256;
    -- Vulkan 1.4
    Max_Global_Priority_Size: constant := 16;

    -- Handle types.
    type Object_Handle is new Interfaces.C.Extensions.opaque_structure_def;
    for Object_Handle'Size use 64;

    type Instance is new Object_Handle;
    type Physical_Device is new Object_Handle;
    type Device is new Object_Handle;
    type Queue is new Object_Handle;
    type Semaphore is new Object_Handle;
    type Command_Buffer is new Object_Handle;
    type Fence is new Object_Handle;
    type Device_Memory is new Object_Handle;
    type Buffer is new Object_Handle;
    type Image is new Object_Handle;
    type Event is new Object_Handle;
    type Query_Pool is new Object_Handle;
    type Buffer_View is new Object_Handle;
    type Image_View is new Object_Handle;
    type Shader_Module is new Object_Handle;
    type Pipeline_Cache is new Object_Handle;
    type Pipeline_Layout is new Object_Handle;
    type Render_Pass is new Object_Handle;
    type Pipeline is new Object_Handle;
    type Descriptor_Set_Layout is new Object_Handle;
    type Sampler is new Object_Handle;
    type Descriptor_Pool is new Object_Handle;
    type Descriptor_Set is new Object_Handle;
    type Framebuffer is new Object_Handle;
    type Command_Pool is new Object_Handle;
    -- Vulkan 1.1
    type Sampler_YCbCr_Conversion is new Object_Handle;
    type Descriptor_Update_Template is new Object_Handle;
    -- Vulkan 1.3
    type Private_Data_Slot is new Object_Handle;

    No_Object_Handle: constant Object_Handle :=
        Object_Handle(System.Null_Address);
    No_Instance: constant Instance := Instance(System.Null_Address);
    No_Physical_Device: constant Physical_Device :=
        Physical_Device(System.Null_Address);
    No_Device: constant Device := Device(System.Null_Address);
    No_Queue: constant Queue := Queue(System.Null_Address);
    No_Semaphore: constant Semaphore := Semaphore(System.Null_Address);
    No_Command_Buffer: constant Command_Buffer :=
        Command_Buffer(System.Null_Address);
    No_Fence: constant Fence := Fence(System.Null_Address);
    No_Device_Memory: constant Device_Memory :=
        Device_Memory(System.Null_Address);
    No_Buffer: constant Buffer := Buffer(System.Null_Address);
    No_Image: constant Image := Image(System.Null_Address);
    No_Event: constant Event := Event(System.Null_Address);
    No_Query_Pool: constant Query_Pool := Query_Pool(System.Null_Address);
    No_Buffer_View: constant Buffer_View := Buffer_View(System.Null_Address);
    No_Image_View: constant Image_View := Image_View(System.Null_Address);
    No_Shader_Module: constant Shader_Module :=
        Shader_Module(System.Null_Address);
    No_Pipeline_Cache: constant Pipeline_Cache :=
        Pipeline_Cache(System.Null_Address);
    No_Pipeline_Layout: constant Pipeline_Layout :=
        Pipeline_Layout(System.Null_Address);
    No_Render_Pass: constant Render_Pass := Render_Pass(System.Null_Address);
    No_Pipeline: constant Pipeline := Pipeline(System.Null_Address);
    No_Descriptor_Set_Layout: constant Descriptor_Set_Layout :=
        Descriptor_Set_Layout(System.Null_Address);
    No_Sampler: constant Sampler := Sampler(System.Null_Address);
    No_Descriptor_Pool: constant Descriptor_Pool :=
        Descriptor_Pool(System.Null_Address);
    No_Descriptor_Set: constant Descriptor_Set :=
        Descriptor_Set(System.Null_Address);
    No_Framebuffer: constant Framebuffer := Framebuffer(System.Null_Address);
    No_Command_Pool: constant Command_Pool := Command_Pool(System.Null_Address);
    -- Vulkan 1.1
    No_Sampler_YCbCr_Conversion: constant Sampler_YCbCr_Conversion :=
        Sampler_YCbCr_Conversion(System.Null_Address);
    No_Descriptor_Update_Template: constant Descriptor_Update_Template :=
        Descriptor_Update_Template(System.Null_Address);
    -- Vulkan 1.3
    No_Private_Data_Slot: constant Private_Data_Slot :=
        Private_Data_Slot(System.Null_Address);

    -- Handle vector types.
    package Physical_Device_Vectors is
        new Ada.Containers.Vectors(Positive, Physical_Device);
    package Semaphore_Vectors is
        new Ada.Containers.Vectors(Positive, Semaphore);
    package Command_Buffer_Vectors is
        new Ada.Containers.Vectors(Positive, Command_Buffer);
    package Fence_Vectors is new Ada.Containers.Vectors(Positive, Fence);
    package Device_Memory_Vectors is
        new Ada.Containers.Vectors(Positive, Device_Memory);
    package Buffer_Vectors is new Ada.Containers.Vectors(Positive, Buffer);
    package Image_Vectors is new Ada.Containers.Vectors(Positive, Image);
    package Event_Vectors is new Ada.Containers.Vectors(Positive, Event);
    package Buffer_View_Vectors is
        new Ada.Containers.Vectors(Positive, Buffer_View);
    package Image_View_Vectors is
        new Ada.Containers.Vectors(Positive, Image_View);
    package Pipeline_Cache_Vectors is
        new Ada.Containers.Vectors(Positive, Pipeline_Cache);
    package Pipeline_Vectors is new Ada.Containers.Vectors(Positive, Pipeline);
    package Descriptor_Set_Layout_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Set_Layout);
    package Sampler_Vectors is new Ada.Containers.Vectors(Positive, Sampler);
    package Descriptor_Set_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Set);
    package Framebuffer_Vectors is
        new Ada.Containers.Vectors(Positive, Framebuffer);

    -- Enumerations.
    type Result is (Incompatible_Shader_Binary,
                    Compression_Exhausted,
                    Invalid_Opaque_Capture_Address,
                    Full_Screen_Exclusive_Mode_Lost,
                    Not_Permitted,
                    Fragmentation,
                    Invalid_DRM_Format_Modifier_Plane_Layout,
                    Incompatible_Version,
                    Invalid_External_Handle,
                    Out_Of_Pool_Memory,
                    Video_Std_Version_Not_Supported,
                    Video_Profile_Codec_Not_Supported,
                    Video_Profile_Format_Not_Supported,
                    Video_Profile_Operation_Not_Supported,
                    Video_Picture_Layout_Not_Supported,
                    Image_Usage_Not_Supported,
                    Invalid_Shader,
                    Validation_Failed,
                    Incompatible_Display,
                    Out_Of_Date,
                    Native_Window_In_Use,
                    Surface_Lost,
                    Unknown,
                    Fragmented_Pool,
                    Format_Not_Supported,
                    Too_Many_Objects,
                    Incompatible_Driver,
                    Feature_Not_Present,
                    Extension_Not_Present,
                    Layer_Not_Present,
                    Memory_Map_Failed,
                    Device_Lost,
                    Initialization_Failed,
                    Out_Of_Device_Memory,
                    Out_Of_Host_Memory,
                    Success,
                    Not_Ready,
                    Timeout,
                    Event_Set,
                    Event_Reset,
                    Incomplete,
                    Suboptimal,
                    Thread_Idle,
                    Thread_Done,
                    Operation_Deferred,
                    Operation_Not_Deferred,
                    Pipeline_Compile_Required)
        with Convention => C;

    for Result'Size use 32;

    for Result use 
        (Incompatible_Shader_Binary => -1_000_482_000,
         Compression_Exhausted => -1_000_338_000,
         Invalid_Opaque_Capture_Address => -1_000_257_000,
         Full_Screen_Exclusive_Mode_Lost => -1_000_255_000,
         Not_Permitted => -1_000_174_001,
         Fragmentation => -1_000_161_000,
         Invalid_DRM_Format_Modifier_Plane_Layout => -1_000_158_000,
         Incompatible_Version => -1_000_150_000,
         Invalid_External_Handle => -1_000_072_003,
         Out_Of_Pool_Memory => -1_000_069_000,
         Video_Std_Version_Not_Supported => -1_000_023_005,
         Video_Profile_Codec_Not_Supported => -1_000_023_004,
         Video_Profile_Format_Not_Supported => -1_000_023_003,
         Video_Profile_Operation_Not_Supported => -1_000_023_002,
         Video_Picture_Layout_Not_Supported => -1_000_023_001,
         Image_Usage_Not_Supported => -1_000_023_000,
         Invalid_Shader => -1_000_012_000,
         Validation_Failed => -1_000_011_001,
         Incompatible_Display => -1_000_003_001,
         Out_Of_Date => -1_000_001_004,
         Native_Window_In_Use => -1_000_000_001,
         Surface_Lost => -1_000_000_000,
         Unknown => -13,
         Fragmented_Pool => -12,
         Format_Not_Supported => -11,
         Too_Many_Objects => -10,
         Incompatible_Driver => -9,
         Feature_Not_Present => -8,
         Extension_Not_Present => -7,
         Layer_Not_Present => -6,
         Memory_Map_Failed => -5,
         Device_Lost => -4,
         Initialization_Failed => -3,
         Out_Of_Device_Memory => -2,
         Out_Of_Host_Memory => -1,
         Success => 0,
         Not_Ready => 1,
         Timeout => 2,
         Event_Set => 3,
         Event_Reset => 4,
         Incomplete => 5,
         Suboptimal => 1_000_001_003,
         Thread_Idle => 1_000_268_000,
         Thread_Done => 1_000_268_001,
         Operation_Deferred => 1_000_268_002,
         Operation_Not_Deferred => 1_000_268_003,
         Pipeline_Compile_Required => 1_000_297_000);

    subtype Error_Result is Result range Result'First .. Result'Pred(Success);

    package Result_Vectors is new Ada.Containers.Vectors(Positive, Result);

    type Structure_Type is 
        (Application_Info_Type,
         Instance_Create_Info_Type,
         Device_Queue_Create_Info_Type,
         Device_Create_Info_Type,
         Submit_Info_Type,
         Memory_Allocate_Info_Type,
         Mapped_Memory_Range_Type,
         Bind_Sparse_Info_Type,
         Fence_Create_Info_Type,
         Semaphore_Create_Info_Type,
         Event_Create_Info_Type,
         Query_Pool_Create_Info_Type,
         Buffer_Create_Info_Type,
         Buffer_View_Create_Info_Type,
         Image_Create_Info_Type,
         Image_View_Create_Info_Type,
         Shader_Module_Create_Info_Type,
         Pipeline_Cache_Create_Info_Type,
         Pipeline_Shader_Stage_Create_Info_Type,
         Pipeline_Vertex_Input_State_Create_Info_Type,
         Pipeline_Input_Assembly_State_Create_Info_Type,
         Pipeline_Tessellation_State_Create_Info_Type,
         Pipeline_Viewport_State_Create_Info_Type,
         Pipeline_Rasterization_State_Create_Info_Type,
         Pipeline_Multisample_State_Create_Info_Type,
         Pipeline_Depth_Stencil_State_Create_Info_Type,
         Pipeline_Color_Blend_State_Create_Info_Type,
         Pipeline_Dynamic_State_Create_Info_Type,
         Graphics_Pipeline_Create_Info_Type,
         Compute_Pipeline_Create_Info_Type,
         Pipeline_Layout_Create_Info_Type,
         Sampler_Create_Info_Type,
         Descriptor_Set_Layout_Create_Info_Type,
         Descriptor_Pool_Create_Info_Type,
         Descriptor_Set_Allocate_Info_Type,
         Write_Descriptor_Set_Type,
         Copy_Descriptor_Set_Type,
         Framebuffer_Create_Info_Type,
         Render_Pass_Create_Info_Type,
         Command_Pool_Create_Info_Type,
         Command_Buffer_Allocate_Info_Type,
         Command_Buffer_Inheritance_Info_Type,
         Command_Buffer_Begin_Info_Type,
         Render_Pass_Begin_Info_Type,
         Buffer_Memory_Barrier_Type,
         Image_Memory_Barrier_Type,
         Memory_Barrier_Type,
         Physical_Device_Vulkan_1_1_Features_Type,
         Physical_Device_Vulkan_1_1_Properties_Type,
         Physical_Device_Vulkan_1_2_Features_Type,
         Physical_Device_Vulkan_1_2_Properties_Type,
         Physical_Device_Vulkan_1_3_Features_Type,
         Physical_Device_Vulkan_1_3_Properties_Type,
         Physical_Device_Vulkan_1_4_Features_Type,
         Physical_Device_Vulkan_1_4_Properties_Type,
         Swapchain_Create_Info_Type,
         Present_Info_Type,
         Display_Mode_Create_Info_Type,
         Display_Surface_Create_Info_Type,
         Display_Present_Info_Type,
         Xlib_Surface_Create_Info_Type,
         Xcb_Surface_Create_Info_Type,
         Wayland_Surface_Create_Info_Type,
         Win32_Surface_Create_Info_Type,
         Pipeline_Rasterization_State_Rasterization_Order_Type,
         Video_Profile_Info_Type,
         Video_Capabilities_Type,
         Video_Picture_Resource_Info_Type,
         Video_Session_Memory_Requirements_Type,
         Bind_Video_Session_Memory_Info_Type,
         Video_Session_Create_Info_Type,
         Video_Session_Parameters_Create_Info_Type,
         Video_Session_Parameters_Update_Info_Type,
         Video_Begin_Coding_Info_Type,
         Video_End_Coding_Info_Type,
         Video_Coding_Control_Info_Type,
         Video_Reference_Slot_Info_Type,
         Queue_Family_Video_Properties_Type,
         Video_Profile_List_Info_Type,
         Physical_Device_Video_Format_Info_Type,
         Video_Format_Properties_Type,
         Queue_Family_Query_Result_Status_Properties_Type,
         Video_Decode_Info_Type,
         Video_Decode_Capabilities_Type,
         Video_Decode_Usage_Info_Type,
         Dedicated_Allocation_Image_Create_Info_Type,
         Dedicated_Allocation_Buffer_Create_Info_Type,
         Dedicated_Allocation_Memory_Allocate_Info_Type,
         Physical_Device_Transform_Feedback_Features_Type,
         Physical_Device_Transform_Feedback_Properties_Type,
         Pipeline_Rasterization_State_Stream_Create_Info_Type,
         Cu_Module_Create_Info_Type,
         Cu_Function_Create_Info_Type,
         Cu_Launch_Info_Type,
         Image_View_Handle_Info_Type,
         Image_View_Address_Properties_Type,
         Video_Encode_H264_Capabilities_Type,
         Video_Encode_H264_Session_Parameters_Create_Info_Type,
         Video_Encode_H264_Session_Parameters_Add_Info_Type,
         Video_Encode_H264_Picture_Info_Type,
         Video_Encode_H264_DPB_Slot_Info_Type,
         Video_Encode_H264_Nalu_Slice_Info_Type,
         Video_Encode_H264_GOP_Remaining_Frame_Info_Type,
         Video_Encode_H264_Profile_Info_Type,
         Video_Encode_H264_Rate_Control_Info_Type,
         Video_Encode_H264_Rate_Control_Layer_Info_Type,
         Video_Encode_H264_Session_Create_Info_Type,
         Video_Encode_H264_Quality_Level_Properties_Type,
         Video_Encode_H264_Session_Parameters_Get_Info_Type,
         Video_Encode_H264_Session_Parameters_Feedback_Info_Type,
         Video_Encode_H265_Capabilities_Type,
         Video_Encode_H265_Session_Parameters_Create_Info_Type,
         Video_Encode_H265_Session_Parameters_Add_Info_Type,
         Video_Encode_H265_Picture_Info_Type,
         Video_Encode_H265_DPB_Slot_Info_Type,
         Video_Encode_H265_Nalu_Slice_Segment_Info_Type,
         Video_Encode_H265_GOP_Remaining_Frame_Info_Type,
         Video_Encode_H265_Profile_Info_Type,
         Video_Encode_H265_Rate_Control_Info_Type,
         Video_Encode_H265_Rate_Control_Layer_Info_Type,
         Video_Encode_H265_Session_Create_Info_Type,
         Video_Encode_H265_Quality_Level_Properties_Type,
         Video_Encode_H265_Session_Parameters_Get_Info_Type,
         Video_Encode_H265_Session_Parameters_Feedback_Info_Type,
         Video_Decode_H264_Capabilities_Type,
         Video_Decode_H264_Picture_Info_Type,
         Video_Decode_H264_Profile_Info_Type,
         Video_Decode_H264_Session_Parameters_Create_Info_Type,
         Video_Decode_H264_Session_Parameters_Add_Info_Type,
         Video_Decode_H264_DPB_Slot_Info_Type,
         Texture_LOD_Gather_Format_Properties_Type,
         Rendering_Info_Type,
         Rendering_Attachment_Info_Type,
         Pipeline_Rendering_Create_Info_Type,
         Physical_Device_Dynamic_Rendering_Features_Type,
         Command_Buffer_Inheritance_Rendering_Info_Type,
         Rendering_Fragment_Shading_Rate_Attachment_Info_Type,
         Rendering_Fragment_Density_Map_Attachment_Info_Type,
         Attachment_Sample_Count_Info_Type,
         Multiview_Per_View_Attributes_Info_Type,
         Physical_Device_Corner_Sampled_Image_Features_Type,
         Render_Pass_Multiview_Create_Info_Type,
         Physical_Device_Multiview_Features_Type,
         Physical_Device_Multiview_Properties_Type,
         Physical_Device_Features_2_Type,
         Physical_Device_Properties_2_Type,
         Format_Properties_2_Type,
         Image_Format_Properties_2_Type,
         Physical_Device_Image_Format_Info_2_Type,
         Queue_Family_Properties_2_Type,
         Physical_Device_Memory_Properties_2_Type,
         Sparse_Image_Format_Properties_2_Type,
         Physical_Device_Sparse_Image_Format_Info_2_Type,
         Memory_Allocate_Flags_Info_Type,
         Device_Group_Render_Pass_Begin_Info_Type,
         Device_Group_Command_Buffer_Begin_Info_Type,
         Device_Group_Submit_Info_Type,
         Device_Group_Bind_Sparse_Info_Type,
         Device_Group_Present_Capabilities_Type,
         Image_Swapchain_Create_Info_Type,
         Bind_Image_Memory_Swapchain_Info_Type,
         Acquire_Next_Image_Info_Type,
         Device_Group_Present_Info_Type,
         Device_Group_Swapchain_Create_Info_Type,
         Bind_Buffer_Memory_Device_Group_Info_Type,
         Bind_Image_Memory_Device_Group_Info_Type,
         Physical_Device_Shader_Draw_Parameter_Features_Type,
         Physical_Device_Texture_Compression_ASTC_HDR_Features_Type,
         Image_View_ASTC_Decode_Mode_Type,
         Physical_Device_ASTC_Decode_Features_Type,
         Pipeline_Robustness_Create_Info_Type,
         Physical_Device_Pipeline_Robustness_Features_Type,
         Physical_Device_Pipeline_Robustness_Properties_Type,
         Physical_Device_Group_Properties_Type,
         Device_Group_Device_Create_Info_Type,
         Physical_Device_External_Image_Format_Info_Type,
         External_Image_Format_Properties_Type,
         Physical_Device_External_Buffer_Info_Type,
         External_Buffer_Properties_Type,
         Physical_Device_ID_Properties_Type,
         External_Memory_Buffer_Create_Info_Type,
         External_Memory_Image_Create_Info_Type,
         Export_Memory_Allocate_Info_Type,
         Import_Memory_FD_Info_Type,
         Memory_FD_Properties_Type,
         Memory_Get_FD_Info_Type,
         Physical_Device_External_Semaphore_Info_Type,
         External_Semaphore_Properties_Type,
         Export_Semaphore_Create_Info_Type,
         Import_Semaphore_FD_Info_Type,
         Semaphore_Get_FD_Info_Type,
         Physical_Device_Push_Descriptor_Properties_Type,
         Command_Buffer_Inheritance_Conditional_Rendering_Info_Type,
         Physical_Device_Conditional_Rendering_Features_Type,
         Conditional_Rendering_Begin_Info_Type,
         Physical_Device_Shader_Float16_Int8_Features_Type,
         Physical_Device_16Bit_Storage_Features_Type,
         Present_Regions_Type,
         Descriptor_Update_Template_Create_Info_Type,
         Pipeline_Viewport_W_Scaling_State_Create_Info_Type,
         Surface_Capabilities_2_EXT_Type,
         Display_Power_Info_Type,
         Device_Event_Info_Type,
         Display_Event_Info_Type,
         Swapchain_Counter_Create_Info_Type,
         Present_Times_Info_Type,
         Physical_Device_Subgroup_Properties_Type,
         Physical_Device_Multiview_Per_View_Attributes_Properties_Type,
         Pipeline_Viewport_Swizzle_State_Create_Info_Type,
         Physical_Device_Discard_Rectangle_Properties_Type,
         Pipeline_Discard_Rectangle_State_Create_Info_Type,
         Physical_Device_Conservative_Rasterization_Properties_Type,
         Pipeline_Rasterization_Conservative_State_Create_Info_Type,
         Physical_Device_Depth_Clip_Enable_Features_Type,
         Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type,
         HDR_Metadata_Type,
         Physical_Device_Imageless_Framebuffer_Features_Type,
         Framebuffer_Attachments_Create_Info_Type,                           
         Framebuffer_Attachment_Image_Info_Type,                             
         Render_Pass_Attachment_Begin_Info_Type,                             
         Attachment_Description_2_Type,                                      
         Attachment_Reference_2_Type,                                        
         Subpass_Description_2_Type,                                         
         Subpass_Dependency_2_Type,                                          
         Render_Pass_Create_Info_2_Type,                                     
         Subpass_Begin_Info_Type,                                            
         Subpass_End_Info_Type,
         Physical_Device_Relaxed_Line_Rasterization_Features_Type,
         Shared_Present_Surface_Capabilities_Type,                           
         Physical_Device_External_Fence_Info_Type,                           
         External_Fence_Properties_Type,                                     
         Export_Fence_Create_Info_Type,                                      
         Import_Fence_FD_Info_Type,                                          
         Fence_Get_FD_Info_Type,                                             
         Physical_Device_Performance_Query_Features_Type,                    
         Physical_Device_Performance_Query_Properties_Type,                  
         Query_Pool_Performance_Create_Info_Type,                            
         Performance_Query_Submit_Info_Type,                                 
         Acquire_Profiling_Lock_Info_Type,                                   
         Performance_Counter_Type,                                           
         Performance_Counter_Description_Type,                               
         Physical_Device_Point_Clipping_Properties_Type,                     
         Render_Pass_Input_Attachment_Aspect_Create_Info_Type,
         Image_View_Usage_Create_Info_Type,                                  
         Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type,
         Physical_Device_Surface_Info_2_Type,                                
         Surface_Capabilities_2_Type,                                        
         Surface_Format_2_Type,                                              
         Physical_Device_Variable_Pointer_Features_Type,                     
         Display_Properties_2_Type,                                          
         Display_Plane_Properties_2_Type,                                    
         Display_Mode_Properties_2_Type,                                     
         Display_Plane_Info_2_Type,                                          
         Display_Plane_Capabilities_2_Type,                                  
         Memory_Dedicated_Requirements_Type,                                 
         Memory_Dedicated_Allocate_Info_Type,                                
         Debug_Utils_Object_Name_Info_Type,                                  
         Debug_Utils_Object_Tag_Info_Type,                                   
         Debug_Utils_Label_Type,                                             
         Debug_Utils_Messenger_Callback_Data_Type,                           
         Debug_Utils_Messenger_Create_Info_Type,                             
         Physical_Device_Sampler_Filter_Minmax_Properties_Type,
         Sampler_Reduction_Mode_Create_Info_Type,                            
         Physical_Device_Inline_Uniform_Block_Features_Type,
         Physical_Device_Inline_Uniform_Block_Properties_Type,
         Write_Descriptor_Set_Inline_Uniform_Block_Type,                     
         Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type,
         Protected_Submit_Info_Type,                                         
         Physical_Device_Protected_Memory_Features_Type,                     
         Physical_Device_Protected_Memory_Properties_Type,                   
         Device_Queue_Info_2_Type,                                           
         Buffer_Memory_Requirements_Info_2_Type,                             
         Image_Memory_Requirements_Info_2_Type,                              
         Image_Sparse_Memory_Requirements_Info_2_Type,                       
         Memory_Requirements_2_Type,                                         
         Sparse_Image_Memory_Requirements_2_Type,                            
         Image_Format_List_Create_Info_Type,                                 
         Sampler_YCbCr_Conversion_Create_Info_Type,                          
         Sampler_YCbCr_Conversion_Info_Type,                                 
         Bind_Image_Plane_Memory_Info_Type,                                  
         Image_Plane_Memory_Requirements_Info_Type,                          
         Physical_Device_Sampler_YCbCr_Conversion_Features_Type,
         Sampler_YCbCr_Conversion_Image_Format_Properties_Type,
         Bind_Buffer_Memory_Info_Type,                                       
         Bind_Image_Memory_Info_Type,                                        
         Descriptor_Set_Layout_Binding_Flags_Create_Info_Type,
         Physical_Device_Descriptor_Indexing_Features_Type,                  
         Physical_Device_Descriptor_Indexing_Properties_Type,
         Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type,
         Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type,
         Physical_Device_Maintenance_3_Properties_Type,
         Descriptor_Set_Layout_Support_Type,
         Device_Queue_Global_Priority_Create_Info_Type,
         Physical_Device_Shader_Subgroup_Extended_Types_Features_Type,
         Physical_Device_8Bit_Storage_Features_Type,
         Physical_Device_Shader_Atomic_Int64_Features_Type,
         Physical_Device_Shader_Clock_Features_Type,
         Calibrated_Timestamp_Info_Type,
         Video_Decode_H265_Capabilities_Type,
         Video_Decode_H265_Session_Parameters_Create_Info_Type,
         Video_Decode_H265_Session_Parameters_Add_Info_Type,
         Video_Decode_H265_Profile_Info_Type,
         Video_Decode_H265_Picture_Info_Type,
         Video_Decode_H265_DPB_Slot_Info_Type,
         Pipeline_Vertex_Input_Divisor_State_Create_Info_Type,
         Physical_Device_Vertex_Attribute_Divisor_Features_Type,
         Pipeline_Creation_Feedback_Create_Info_Type,
         Physical_Device_Driver_Properties_Type,
         Physical_Device_Float_Controls_Properties_Type,
         Physical_Device_Depth_Stencil_Resolve_Properties_Type,
         Subpass_Description_Depth_Stencil_Resolve_Type,
         Physical_Device_Fragment_Shader_Barycentric_Features_Type,
         Physical_Device_Timeline_Semaphore_Features_Type,
         Physical_Device_Timeline_Semaphore_Properties_Type,
         Semaphore_Type_Create_Info_Type,
         Timeline_Semaphore_Submit_Info_Type,
         Semaphore_Wait_Info_Type,
         Semaphore_Signal_Info_Type,
         Physical_Device_Vulkan_Memory_Model_Features_Type,
         Physical_Device_Shader_Terminate_Invocation_Features_Type,
         Metal_Surface_Create_Info_Type,
         Physical_Device_Scalar_Block_Layout_Features_Type,
         Physical_Device_Subgroup_Size_Control_Properties_Type,
         Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type,
         Physical_Device_Subgroup_Size_Control_Features_Type,
         Fragment_Shading_Rate_Attachment_Info_Type,
         Pipeline_Fragment_Shading_Rate_State_Create_Info_Type,
         Physical_Device_Fragment_Shading_Rate_Properties_Type,
         Physical_Device_Fragment_Shading_Rate_Features_Type,
         Physical_Device_Fragment_Shading_Rate_Type,
         Physical_Device_Dynamic_Rendering_Local_Read_Features_Type,
         Rendering_Attachment_Location_Info_Type,
         Rendering_Input_Attachment_Index_Info_Type,
         Physical_Device_Shader_Quad_Control_Features_Type,
         Surface_Protected_Capabilities_Type,
         Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type,
         Attachment_Reference_Stencil_Layout_Type,
         Attachment_Description_Stencil_Layout_Type,
         Buffer_Device_Address_Info_Type,
         Physical_Device_Tool_Properties_Type,
         Image_Stencil_Usage_Create_Info_Type,
         Physical_Device_Present_Wait_Features_Type,
         Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type,
         Physical_Device_Buffer_Device_Address_Features_Type,
         Buffer_Opaque_Capture_Address_Create_Info_Type,
         Memory_Opaque_Capture_Address_Allocate_Info_Type,
         Device_Memory_Opaque_Capture_Address_Info_Type,
         Physical_Device_Line_Rasterization_Features_Type,
         Pipeline_Rasterization_Line_State_Create_Info_Type,
         Physical_Device_Line_Rasterization_Properties_Type,
         Physical_Device_Host_Query_Reset_Features_Type,
         Physical_Device_Index_Type_Uint8_Features_Type,
         Physical_Device_Pipeline_Executable_Properties_Features_Type,
         Pipeline_Info_Type,
         Pipeline_Executable_Properties_Type,
         Pipeline_Executable_Info_Type,
         Pipeline_Executable_Statistic_Type,
         Pipeline_Executable_Internal_Representation_Type,
         Physical_Device_Host_Image_Copy_Features_Type,
         Physical_Device_Host_Image_Copy_Properties_Type,
         Memory_To_Image_Copy_Type,
         Image_To_Memory_Copy_Type,
         Copy_Image_To_Memory_Info_Type,
         Copy_Memory_To_Image_Info_Type,
         Host_Image_Layout_Transition_Info_Type,
         Copy_Image_To_Image_Info_Type,
         Subresource_Host_Memcpy_Size_Type,
         Host_Image_Copy_Device_Performance_Query_Type,
         Memory_Map_Info_Type,
         Memory_Unmap_Info_Type,
         Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type,
         Physical_Device_Shader_Integer_Dot_Product_Features_Type,
         Physical_Device_Shader_Integer_Dot_Product_Properties_Type,
         Physical_Device_Texel_Buffer_Alignment_Properties_Type,
         Pipeline_Library_Create_Info_Type,
         Present_ID_Type,
         Physical_Device_Present_ID_Features_Type,
         Physical_Device_Private_Data_Features_Type,
         Device_Private_Data_Create_Info_Type,
         Private_Data_Slot_Create_Info_Type,
         Physical_Device_Pipeline_Creation_Cache_Control_Features_Type,
         Video_Encode_Info_Type,
         Video_Encode_Rate_Control_Info_Type,
         Video_Encode_Rate_Control_Layer_Info_Type,
         Video_Encode_Capabilities_Type,
         Video_Encode_Usage_Info_Type,
         Query_Pool_Video_Encode_Feedback_Create_Info_Type,
         Physical_Device_Video_Encode_Quality_Level_Info_Type,
         Video_Encode_Quality_Level_Properties_Type,
         Video_Encode_Quality_Level_Info_Type,
         Video_Encode_Session_Parameters_Get_Info_Type,
         Video_Encode_Session_Parameters_Feedback_Info_Type,
         Memory_Barrier_2_Type,
         Buffer_Memory_Barrier_2_Type,
         Image_Memory_Barrier_2_Type,
         Dependency_Info_Type,
         Submit_Info_2_Type,
         Semaphore_Submit_Info_Type,
         Command_Buffer_Submit_Info_Type,
         Physical_Device_Synchronization_2_Features_Type,
         Queue_Family_Checkpoint_Properties_2_Type,
         Checkpoint_Data_2_Type,
         Physical_Device_Fragment_Shader_Barycentric_Properties_Type,
         Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type,
         Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type,
         Physical_Device_Image_Robustness_Features_Type,
         Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type,
         Copy_Buffer_Info_2_Type,
         Copy_Image_Info_2_Type,
         Copy_Buffer_To_Image_Info_2_Type,
         Copy_Image_To_Buffer_Info_2_Type,
         Blit_Image_Info_2_Type,
         Resolve_Image_Info_2_Type,
         Buffer_Copy_2_Type,
         Image_Copy_2_Type,
         Image_Blit_2_Type,
         Buffer_Image_Copy_2_Type,
         Image_Resolve_2_Type,
         Subresource_Layout_2_Type,
         Image_Subresource_2_Type,
         Format_Properties_3_Type,
         Physical_Device_Ray_Tracing_Maintenance_1_Features_Type,
         Physical_Device_Global_Priority_Query_Features_Type,
         Queue_Family_Global_Priority_Properties_Type,
         Physical_Device_Maintenance_4_Features_Type,
         Physical_Device_Maintenance_4_Properties_Type,
         Device_Buffer_Memory_Requirements_Type,
         Device_Image_Memory_Requirements_Type,
         Physical_Device_Shader_Subgroup_Rotate_Features_Type,
         Physical_Device_Shader_Maximal_Reconvergence_Features_Type,
         Physical_Device_Pipeline_Protected_Access_Features_Type,
         Physical_Device_Maintenance_5_Features_Type,
         Physical_Device_Maintenance_5_Properties_Type,
         Rendering_Area_Info_Type,
         Device_Image_Subresource_Info_Type,
         Pipeline_Create_Flags_2_Create_Info_Type,
         Buffer_Usage_Flags_2_Create_Info_Type,
         Physical_Device_Ray_Tracing_Position_Fetch_Features_Type,
         Physical_Device_Cooperative_Matrix_Features_Type,
         Cooperative_Matrix_Properties_Type,
         Physical_Device_Cooperative_Matrix_Properties_Type,
         Video_Decode_AV1_Capabilities_Type,
         Video_Decode_AV1_Picture_Info_Type,
         Video_Decode_AV1_Profile_Info_Type,
         Video_Decode_AV1_Session_Parameters_Create_Info_Type,
         Video_Decode_AV1_DPB_Slot_Info_Type,
         Physical_Device_Video_Maintenance_1_Features_Type,
         Video_Inline_Query_Info_Type,
         Physical_Device_Vertex_Attribute_Divisor_Properties_Type,
         Physical_Device_Shader_Float_Controls_2_Features_Type,
         Physical_Device_Shader_Expect_Assume_Features_Type,
         Physical_Device_Maintenance_6_Features_Type,
         Physical_Device_Maintenance_6_Properties_Type,
         Bind_Memory_Status_Type,
         Bind_Descriptor_Sets_Info_Type,
         Push_Constants_Info_Type,
         Push_Descriptor_Set_Info_Type,
         Push_Descriptor_Set_With_Template_Info_Type,
         Set_Descriptor_Buffer_Offsets_Info_Type,
         Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type)
        with Convention => C;

    for Structure_Type'Size use 32;

    for Structure_Type use
        (Application_Info_Type => 0,
         Instance_Create_Info_Type => 1,
         Device_Queue_Create_Info_Type => 2,
         Device_Create_Info_Type => 3,
         Submit_Info_Type => 4,
         Memory_Allocate_Info_Type => 5,
         Mapped_Memory_Range_Type => 6,
         Bind_Sparse_Info_Type => 7,
         Fence_Create_Info_Type => 8,
         Semaphore_Create_Info_Type => 9,
         Event_Create_Info_Type => 10,
         Query_Pool_Create_Info_Type => 11,
         Buffer_Create_Info_Type => 12,
         Buffer_View_Create_Info_Type => 13,
         Image_Create_Info_Type => 14,
         Image_View_Create_Info_Type => 15,
         Shader_Module_Create_Info_Type => 16,
         Pipeline_Cache_Create_Info_Type => 17,
         Pipeline_Shader_Stage_Create_Info_Type => 18,
         Pipeline_Vertex_Input_State_Create_Info_Type => 19,
         Pipeline_Input_Assembly_State_Create_Info_Type => 20,
         Pipeline_Tessellation_State_Create_Info_Type => 21,
         Pipeline_Viewport_State_Create_Info_Type => 22,
         Pipeline_Rasterization_State_Create_Info_Type => 23,
         Pipeline_Multisample_State_Create_Info_Type => 24,
         Pipeline_Depth_Stencil_State_Create_Info_Type => 25,
         Pipeline_Color_Blend_State_Create_Info_Type => 26,
         Pipeline_Dynamic_State_Create_Info_Type => 27,
         Graphics_Pipeline_Create_Info_Type => 28,
         Compute_Pipeline_Create_Info_Type => 29,
         Pipeline_Layout_Create_Info_Type => 30,
         Sampler_Create_Info_Type => 31,
         Descriptor_Set_Layout_Create_Info_Type => 32,
         Descriptor_Pool_Create_Info_Type => 33,
         Descriptor_Set_Allocate_Info_Type => 34,
         Write_Descriptor_Set_Type => 35,
         Copy_Descriptor_Set_Type => 36,
         Framebuffer_Create_Info_Type => 37,
         Render_Pass_Create_Info_Type => 38,
         Command_Pool_Create_Info_Type => 39,
         Command_Buffer_Allocate_Info_Type => 40,
         Command_Buffer_Inheritance_Info_Type => 41,
         Command_Buffer_Begin_Info_Type => 42,
         Render_Pass_Begin_Info_Type => 43,
         Buffer_Memory_Barrier_Type => 44,
         Image_Memory_Barrier_Type => 45,
         Memory_Barrier_Type => 46,
         Physical_Device_Vulkan_1_1_Features_Type => 49,
         Physical_Device_Vulkan_1_1_Properties_Type => 50,
         Physical_Device_Vulkan_1_2_Features_Type => 51,
         Physical_Device_Vulkan_1_2_Properties_Type => 52,
         Physical_Device_Vulkan_1_3_Features_Type => 53,
         Physical_Device_Vulkan_1_3_Properties_Type => 54,
         Physical_Device_Vulkan_1_4_Features_Type => 55,
         Physical_Device_Vulkan_1_4_Properties_Type => 56,
         Swapchain_Create_Info_Type => 1_000_001_000,
         Present_Info_Type => 1_000_001_001,
         Display_Mode_Create_Info_Type => 1_000_002_000,
         Display_Surface_Create_Info_Type => 1_000_002_001,
         Display_Present_Info_Type => 1_000_003_000,
         Xlib_Surface_Create_Info_Type => 1_000_004_000,
         Xcb_Surface_Create_Info_Type => 1_000_005_000,
         Wayland_Surface_Create_Info_Type => 1_000_006_000,
         Win32_Surface_Create_Info_Type => 1_000_009_000,
         Pipeline_Rasterization_State_Rasterization_Order_Type => 1_000_018_000,
         Video_Profile_Info_Type => 1_000_023_000,
         Video_Capabilities_Type => 1_000_023_001,
         Video_Picture_Resource_Info_Type => 1_000_023_002,
         Video_Session_Memory_Requirements_Type => 1_000_023_003,
         Bind_Video_Session_Memory_Info_Type => 1_000_023_004,
         Video_Session_Create_Info_Type => 1_000_023_005,
         Video_Session_Parameters_Create_Info_Type => 1_000_023_006,
         Video_Session_Parameters_Update_Info_Type => 1_000_023_007,
         Video_Begin_Coding_Info_Type => 1_000_023_008,
         Video_End_Coding_Info_Type => 1_000_023_009,
         Video_Coding_Control_Info_Type => 1_000_023_010,
         Video_Reference_Slot_Info_Type => 1_000_023_011,
         Queue_Family_Video_Properties_Type => 1_000_023_012,
         Video_Profile_List_Info_Type => 1_000_023_013,
         Physical_Device_Video_Format_Info_Type => 1_000_023_014,
         Video_Format_Properties_Type => 1_000_023_015,
         Queue_Family_Query_Result_Status_Properties_Type => 1_000_023_016,
         Video_Decode_Info_Type => 1_000_024_000,
         Video_Decode_Capabilities_Type => 1_000_024_001,
         Video_Decode_Usage_Info_Type => 1_000_024_002,
         Dedicated_Allocation_Image_Create_Info_Type => 1_000_026_000,
         Dedicated_Allocation_Buffer_Create_Info_Type => 1_000_026_001,
         Dedicated_Allocation_Memory_Allocate_Info_Type => 1_000_026_002,
         Physical_Device_Transform_Feedback_Features_Type => 1_000_028_000,
         Physical_Device_Transform_Feedback_Properties_Type => 1_000_028_001,
         Pipeline_Rasterization_State_Stream_Create_Info_Type => 1_000_028_002,
         Cu_Module_Create_Info_Type => 1_000_029_000,
         Cu_Function_Create_Info_Type => 1_000_029_001,
         Cu_Launch_Info_Type => 1_000_029_002,
         Image_View_Handle_Info_Type => 1_000_030_000,
         Image_View_Address_Properties_Type => 1_000_030_001,
         Video_Encode_H264_Capabilities_Type => 1_000_038_000,
         Video_Encode_H264_Session_Parameters_Create_Info_Type => 1_000_038_001,
         Video_Encode_H264_Session_Parameters_Add_Info_Type => 1_000_038_002,
         Video_Encode_H264_Picture_Info_Type => 1_000_038_003,
         Video_Encode_H264_DPB_Slot_Info_Type => 1_000_038_004,
         Video_Encode_H264_Nalu_Slice_Info_Type => 1_000_038_005,
         Video_Encode_H264_GOP_Remaining_Frame_Info_Type => 1_000_038_006,
         Video_Encode_H264_Profile_Info_Type => 1_000_038_007,
         Video_Encode_H264_Rate_Control_Info_Type => 1_000_038_008,
         Video_Encode_H264_Rate_Control_Layer_Info_Type => 1_000_038_009,
         Video_Encode_H264_Session_Create_Info_Type => 1_000_038_010,
         Video_Encode_H264_Quality_Level_Properties_Type => 1_000_038_011,
         Video_Encode_H264_Session_Parameters_Get_Info_Type => 1_000_038_012,
         Video_Encode_H264_Session_Parameters_Feedback_Info_Type
            => 1_000_038_013,
         Video_Encode_H265_Capabilities_Type => 1_000_039_000,
         Video_Encode_H265_Session_Parameters_Create_Info_Type => 1_000_039_001,
         Video_Encode_H265_Session_Parameters_Add_Info_Type => 1_000_039_002,
         Video_Encode_H265_Picture_Info_Type => 1_000_039_003,
         Video_Encode_H265_DPB_Slot_Info_Type => 1_000_039_004,
         Video_Encode_H265_Nalu_Slice_Segment_Info_Type => 1_000_039_005,
         Video_Encode_H265_GOP_Remaining_Frame_Info_Type => 1_000_039_006,
         Video_Encode_H265_Profile_Info_Type => 1_000_039_007,
         Video_Encode_H265_Rate_Control_Info_Type => 1_000_039_009,
         Video_Encode_H265_Rate_Control_Layer_Info_Type => 1_000_039_010,
         Video_Encode_H265_Session_Create_Info_Type => 1_000_039_011,
         Video_Encode_H265_Quality_Level_Properties_Type => 1_000_039_012,
         Video_Encode_H265_Session_Parameters_Get_Info_Type => 1_000_039_013,
         Video_Encode_H265_Session_Parameters_Feedback_Info_Type
            => 1_000_039_014,
         Video_Decode_H264_Capabilities_Type => 1_000_040_000,
         Video_Decode_H264_Picture_Info_Type => 1_000_040_001,
         Video_Decode_H264_Profile_Info_Type => 1_000_040_003,
         Video_Decode_H264_Session_Parameters_Create_Info_Type => 1_000_040_004,
         Video_Decode_H264_Session_Parameters_Add_Info_Type => 1_000_040_005,
         Video_Decode_H264_DPB_Slot_Info_Type => 1_000_040_006,
         Texture_LOD_Gather_Format_Properties_Type => 1_000_041_000,
         Rendering_Info_Type => 1_000_044_000,
         Rendering_Attachment_Info_Type => 1_000_044_001,
         Pipeline_Rendering_Create_Info_Type => 1_000_044_002,
         Physical_Device_Dynamic_Rendering_Features_Type => 1_000_044_003,
         Command_Buffer_Inheritance_Rendering_Info_Type => 1_000_044_004,
         Rendering_Fragment_Shading_Rate_Attachment_Info_Type => 1_000_044_006,
         Rendering_Fragment_Density_Map_Attachment_Info_Type => 1_000_044_007,
         Attachment_Sample_Count_Info_Type => 1_000_044_008,
         Multiview_Per_View_Attributes_Info_Type => 1_000_044_009,
         Physical_Device_Corner_Sampled_Image_Features_Type => 1_000_050_000,
         Render_Pass_Multiview_Create_Info_Type => 1_000_053_000,
         Physical_Device_Multiview_Features_Type => 1_000_053_001,
         Physical_Device_Multiview_Properties_Type => 1_000_053_002,
         Physical_Device_Features_2_Type => 1_000_059_000,
         Physical_Device_Properties_2_Type => 1_000_059_001,
         Format_Properties_2_Type => 1_000_059_002,
         Image_Format_Properties_2_Type => 1_000_059_003,
         Physical_Device_Image_Format_Info_2_Type => 1_000_059_004,
         Queue_Family_Properties_2_Type => 1_000_059_005,
         Physical_Device_Memory_Properties_2_Type => 1_000_059_006,
         Sparse_Image_Format_Properties_2_Type => 1_000_059_007,
         Physical_Device_Sparse_Image_Format_Info_2_Type => 1_000_059_008,
         Memory_Allocate_Flags_Info_Type => 1_000_060_000,
         Device_Group_Render_Pass_Begin_Info_Type => 1_000_060_003,
         Device_Group_Command_Buffer_Begin_Info_Type => 1_000_060_004,
         Device_Group_Submit_Info_Type => 1_000_060_005,
         Device_Group_Bind_Sparse_Info_Type => 1_000_060_006,
         Device_Group_Present_Capabilities_Type => 1_000_060_007,
         Image_Swapchain_Create_Info_Type => 1_000_060_008,
         Bind_Image_Memory_Swapchain_Info_Type => 1_000_060_009,
         Acquire_Next_Image_Info_Type => 1_000_060_010,
         Device_Group_Present_Info_Type => 1_000_060_011,
         Device_Group_Swapchain_Create_Info_Type => 1_000_060_012,
         Bind_Buffer_Memory_Device_Group_Info_Type => 1_000_060_013,
         Bind_Image_Memory_Device_Group_Info_Type => 1_000_060_014,
         Physical_Device_Shader_Draw_Parameter_Features_Type => 1_000_063_000,
         Physical_Device_Texture_Compression_ASTC_HDR_Features_Type =>
            1_000_066_000,
         Image_View_ASTC_Decode_Mode_Type => 1_000_067_000,
         Physical_Device_ASTC_Decode_Features_Type => 1_000_067_001,
         Pipeline_Robustness_Create_Info_Type => 1_000_068_000,
         Physical_Device_Pipeline_Robustness_Features_Type => 1_000_068_001,
         Physical_Device_Pipeline_Robustness_Properties_Type => 1_000_068_002,
         Physical_Device_Group_Properties_Type => 1_000_070_000,
         Device_Group_Device_Create_Info_Type => 1_000_070_001,
         Physical_Device_External_Image_Format_Info_Type => 1_000_071_000,
         External_Image_Format_Properties_Type => 1_000_071_001,
         Physical_Device_External_Buffer_Info_Type => 1_000_071_002,
         External_Buffer_Properties_Type => 1_000_071_003,
         Physical_Device_ID_Properties_Type => 1_000_071_004,
         External_Memory_Buffer_Create_Info_Type => 1_000_072_000,
         External_Memory_Image_Create_Info_Type => 1_000_072_001,
         Export_Memory_Allocate_Info_Type => 1_000_072_002,
         Import_Memory_FD_Info_Type => 1_000_074_000,
         Memory_FD_Properties_Type => 1_000_074_001,
         Memory_Get_FD_Info_Type => 1_000_074_002,
         Physical_Device_External_Semaphore_Info_Type => 1_000_076_000,
         External_Semaphore_Properties_Type => 1_000_076_001,
         Export_Semaphore_Create_Info_Type => 1_000_077_000,
         Import_Semaphore_FD_Info_Type => 1_000_079_000,
         Semaphore_Get_FD_Info_Type => 1_000_079_001,
         Physical_Device_Push_Descriptor_Properties_Type => 1_000_080_000,
         Command_Buffer_Inheritance_Conditional_Rendering_Info_Type
            => 1_000_081_000,
         Physical_Device_Conditional_Rendering_Features_Type => 1_000_081_001,
         Conditional_Rendering_Begin_Info_Type => 1_000_081_002,
         Physical_Device_Shader_Float16_Int8_Features_Type => 1_000_082_000,
         Physical_Device_16Bit_Storage_Features_Type => 1_000_083_000,
         Present_Regions_Type => 1_000_084_000,
         Descriptor_Update_Template_Create_Info_Type => 1_000_085_000,
         Pipeline_Viewport_W_Scaling_State_Create_Info_Type => 1_000_087_000,
         Surface_Capabilities_2_EXT_Type => 1_000_090_000,
         Display_Power_Info_Type => 1_000_091_000,
         Device_Event_Info_Type => 1_000_091_001,
         Display_Event_Info_Type => 1_000_091_002,
         Swapchain_Counter_Create_Info_Type => 1_000_091_003,
         Present_Times_Info_Type => 1_000_092_000,
         Physical_Device_Subgroup_Properties_Type => 1_000_094_000,
         Physical_Device_Multiview_Per_View_Attributes_Properties_Type
            => 1_000_097_000,
         Pipeline_Viewport_Swizzle_State_Create_Info_Type => 1_000_098_000,
         Physical_Device_Discard_Rectangle_Properties_Type => 1_000_099_000,
         Pipeline_Discard_Rectangle_State_Create_Info_Type => 1_000_099_001,
         Physical_Device_Conservative_Rasterization_Properties_Type
            => 1_000_101_000,
         Pipeline_Rasterization_Conservative_State_Create_Info_Type
            => 1_000_101_001,
         Physical_Device_Depth_Clip_Enable_Features_Type => 1_000_102_000,
         Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type
            => 1_000_102_001,
         HDR_Metadata_Type => 1_000_105_000,
         Physical_Device_Imageless_Framebuffer_Features_Type => 1_000_108_000,
         Framebuffer_Attachments_Create_Info_Type => 1_000_108_001,
         Framebuffer_Attachment_Image_Info_Type => 1_000_108_002,
         Render_Pass_Attachment_Begin_Info_Type => 1_000_108_003,
         Attachment_Description_2_Type => 1_000_109_000,
         Attachment_Reference_2_Type => 1_000_109_001,
         Subpass_Description_2_Type => 1_000_109_002,
         Subpass_Dependency_2_Type => 1_000_109_003,
         Render_Pass_Create_Info_2_Type => 1_000_109_004,
         Subpass_Begin_Info_Type => 1_000_109_005,
         Subpass_End_Info_Type => 1_000_109_006,
         Physical_Device_Relaxed_Line_Rasterization_Features_Type
            => 1_000_110_000,
         Shared_Present_Surface_Capabilities_Type => 1_000_111_000,
         Physical_Device_External_Fence_Info_Type => 1_000_112_000,
         External_Fence_Properties_Type => 1_000_112_001,
         Export_Fence_Create_Info_Type => 1_000_113_000,
         Import_Fence_FD_Info_Type => 1_000_115_000,
         Fence_Get_FD_Info_Type => 1_000_115_001,
         Physical_Device_Performance_Query_Features_Type => 1_000_116_000,
         Physical_Device_Performance_Query_Properties_Type => 1_000_116_001,
         Query_Pool_Performance_Create_Info_Type => 1_000_116_002,
         Performance_Query_Submit_Info_Type => 1_000_116_003,
         Acquire_Profiling_Lock_Info_Type => 1_000_116_004,
         Performance_Counter_Type => 1_000_116_005,
         Performance_Counter_Description_Type => 1_000_116_006,
         Physical_Device_Point_Clipping_Properties_Type => 1_000_117_000, 
         Render_Pass_Input_Attachment_Aspect_Create_Info_Type => 1_000_117_001,
         Image_View_Usage_Create_Info_Type => 1_000_117_002,
         Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type
            => 1_000_117_003,
         Physical_Device_Surface_Info_2_Type => 1_000_119_000,
         Surface_Capabilities_2_Type => 1_000_119_001,
         Surface_Format_2_Type => 1_000_119_002,
         Physical_Device_Variable_Pointer_Features_Type => 1_000_120_000,
         Display_Properties_2_Type => 1_000_121_000,
         Display_Plane_Properties_2_Type => 1_000_121_001,
         Display_Mode_Properties_2_Type => 1_000_121_002,
         Display_Plane_Info_2_Type => 1_000_121_003,
         Display_Plane_Capabilities_2_Type => 1_000_121_004,
         Memory_Dedicated_Requirements_Type => 1_000_127_000,
         Memory_Dedicated_Allocate_Info_Type => 1_000_127_001,
         Debug_Utils_Object_Name_Info_Type => 1_000_128_000,
         Debug_Utils_Object_Tag_Info_Type => 1_000_128_001,
         Debug_Utils_Label_Type => 1_000_128_002,
         Debug_Utils_Messenger_Callback_Data_Type => 1_000_128_003,
         Debug_Utils_Messenger_Create_Info_Type => 1_000_128_004,
         Physical_Device_Sampler_Filter_Minmax_Properties_Type => 1_000_130_000,
         Sampler_Reduction_Mode_Create_Info_Type => 1_000_130_001,
         Physical_Device_Inline_Uniform_Block_Features_Type => 1_000_138_000,
         Physical_Device_Inline_Uniform_Block_Properties_Type => 1_000_138_001,
         Write_Descriptor_Set_Inline_Uniform_Block_Type => 1_000_138_002,
         Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type => 1_000_138_003,
         Protected_Submit_Info_Type => 1_000_145_000,
         Physical_Device_Protected_Memory_Features_Type => 1_000_145_001,
         Physical_Device_Protected_Memory_Properties_Type => 1_000_145_002,
         Device_Queue_Info_2_Type => 1_000_145_003,
         Buffer_Memory_Requirements_Info_2_Type => 1_000_146_000,
         Image_Memory_Requirements_Info_2_Type => 1_000_146_001,
         Image_Sparse_Memory_Requirements_Info_2_Type => 1_000_146_002,
         Memory_Requirements_2_Type => 1_000_146_003,
         Sparse_Image_Memory_Requirements_2_Type => 1_000_146_004,
         Image_Format_List_Create_Info_Type => 1_000_147_000,
         Sampler_YCbCr_Conversion_Create_Info_Type => 1_000_156_000,
         Sampler_YCbCr_Conversion_Info_Type => 1_000_156_001,
         Bind_Image_Plane_Memory_Info_Type => 1_000_156_002,
         Image_Plane_Memory_Requirements_Info_Type => 1_000_156_003,
         Physical_Device_Sampler_YCbCr_Conversion_Features_Type =>
            1_000_156_004,
         Sampler_YCbCr_Conversion_Image_Format_Properties_Type => 1_000_156_005,
         Bind_Buffer_Memory_Info_Type => 1_000_157_000,
         Bind_Image_Memory_Info_Type => 1_000_157_001,
         Descriptor_Set_Layout_Binding_Flags_Create_Info_Type => 1_000_161_000,
         Physical_Device_Descriptor_Indexing_Features_Type => 1_000_161_001,
         Physical_Device_Descriptor_Indexing_Properties_Type => 1_000_161_002,
         Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type =>
            1_000_161_003,
         Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type =>
            1_000_161_004,
         Physical_Device_Maintenance_3_Properties_Type => 1_000_168_000,
         Descriptor_Set_Layout_Support_Type => 1_000_168_001,
         Device_Queue_Global_Priority_Create_Info_Type => 1_000_174_000,
         Physical_Device_Shader_Subgroup_Extended_Types_Features_Type =>
            1_000_175_000,
         Physical_Device_8Bit_Storage_Features_Type => 1_000_177_000,
         Physical_Device_Shader_Atomic_Int64_Features_Type => 1_000_180_000,
         Physical_Device_Shader_Clock_Features_Type => 1_000_181_000,
         Calibrated_Timestamp_Info_Type => 1_000_184_000,
         Video_Decode_H265_Capabilities_Type => 1_000_187_000,
         Video_Decode_H265_Session_Parameters_Create_Info_Type => 1_000_187_001,
         Video_Decode_H265_Session_Parameters_Add_Info_Type => 1_000_187_002,
         Video_Decode_H265_Profile_Info_Type => 1_000_187_003,
         Video_Decode_H265_Picture_Info_Type => 1_000_187_004,
         Video_Decode_H265_DPB_Slot_Info_Type => 1_000_187_005,
         Pipeline_Vertex_Input_Divisor_State_Create_Info_Type => 1_000_190_001,
         Physical_Device_Vertex_Attribute_Divisor_Features_Type =>
            1_000_190_002,
         Pipeline_Creation_Feedback_Create_Info_Type => 1_000_192_000,
         Physical_Device_Driver_Properties_Type => 1_000_196_000,
         Physical_Device_Float_Controls_Properties_Type => 1_000_197_000,
         Physical_Device_Depth_Stencil_Resolve_Properties_Type => 1_000_199_000,
         Subpass_Description_Depth_Stencil_Resolve_Type => 1_000_199_001,
         Physical_Device_Fragment_Shader_Barycentric_Features_Type =>
            1_000_203_000,
         Physical_Device_Timeline_Semaphore_Features_Type => 1_000_207_000,
         Physical_Device_Timeline_Semaphore_Properties_Type => 1_000_207_001,
         Semaphore_Type_Create_Info_Type => 1_000_207_002,
         Timeline_Semaphore_Submit_Info_Type => 1_000_207_003,
         Semaphore_Wait_Info_Type => 1_000_207_004,
         Semaphore_Signal_Info_Type => 1_000_207_005,
         Physical_Device_Vulkan_Memory_Model_Features_Type => 1_000_211_000,
         Physical_Device_Shader_Terminate_Invocation_Features_Type =>
            1_000_215_000,
         Metal_Surface_Create_Info_Type => 1_000_217_000,
         Physical_Device_Scalar_Block_Layout_Features_Type => 1_000_221_000,
         Physical_Device_Subgroup_Size_Control_Properties_Type => 1_000_225_000,
         Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type =>
            1_000_225_001,
         Physical_Device_Subgroup_Size_Control_Features_Type => 1_000_225_002,
         Fragment_Shading_Rate_Attachment_Info_Type => 1_000_226_000,
         Pipeline_Fragment_Shading_Rate_State_Create_Info_Type => 1_000_226_001,
         Physical_Device_Fragment_Shading_Rate_Properties_Type =>
            1_000_226_002,
         Physical_Device_Fragment_Shading_Rate_Features_Type => 1_000_226_003,
         Physical_Device_Fragment_Shading_Rate_Type => 1_000_226_004,
         Physical_Device_Dynamic_Rendering_Local_Read_Features_Type =>
            1_000_232_000,
         Rendering_Attachment_Location_Info_Type => 1_000_232_001,
         Rendering_Input_Attachment_Index_Info_Type => 1_000_232_002,
         Physical_Device_Shader_Quad_Control_Features_Type => 1_000_235_000,
         Surface_Protected_Capabilities_Type => 1_000_239_000,
         Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type =>
            1_000_241_000,
         Attachment_Reference_Stencil_Layout_Type => 1_000_241_001,
         Attachment_Description_Stencil_Layout_Type => 1_000_241_002,
         Buffer_Device_Address_Info_Type => 1_000_244_001,
         Physical_Device_Tool_Properties_Type => 1_000_245_000,
         Image_Stencil_Usage_Create_Info_Type => 1_000_246_000,
         Physical_Device_Present_Wait_Features_Type => 1_000_248_000,
         Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type =>
            1_000_253_000,
         Physical_Device_Buffer_Device_Address_Features_Type => 1_000_257_000,
         Buffer_Opaque_Capture_Address_Create_Info_Type => 1_000_257_002,
         Memory_Opaque_Capture_Address_Allocate_Info_Type => 1_000_257_003,
         Device_Memory_Opaque_Capture_Address_Info_Type => 1_000_257_004,
         Physical_Device_Line_Rasterization_Features_Type => 1_000_259_000,
         Pipeline_Rasterization_Line_State_Create_Info_Type => 1_000_259_001,
         Physical_Device_Line_Rasterization_Properties_Type => 1_000_259_002,
         Physical_Device_Host_Query_Reset_Features_Type => 1_000_261_000,
         Physical_Device_Index_Type_Uint8_Features_Type => 1_000_265_000,
         Physical_Device_Pipeline_Executable_Properties_Features_Type =>
            1_000_269_000,
         Pipeline_Info_Type => 1_000_269_001,
         Pipeline_Executable_Properties_Type => 1_000_269_002,
         Pipeline_Executable_Info_Type => 1_000_269_003,
         Pipeline_Executable_Statistic_Type => 1_000_269_004,
         Pipeline_Executable_Internal_Representation_Type => 1_000_269_005,
         Physical_Device_Host_Image_Copy_Features_Type => 1_000_270_000,
         Physical_Device_Host_Image_Copy_Properties_Type => 1_000_270_001,
         Memory_To_Image_Copy_Type => 1_000_270_002,
         Image_To_Memory_Copy_Type => 1_000_270_003,
         Copy_Image_To_Memory_Info_Type => 1_000_270_004,
         Copy_Memory_To_Image_Info_Type => 1_000_270_005,
         Host_Image_Layout_Transition_Info_Type => 1_000_270_006,
         Copy_Image_To_Image_Info_Type => 1_000_270_007,
         Subresource_Host_Memcpy_Size_Type => 1_000_270_008,
         Host_Image_Copy_Device_Performance_Query_Type => 1_000_270_009,
         Memory_Map_Info_Type => 1_000_271_000,
         Memory_Unmap_Info_Type => 1_000_271_001,
         Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type =>
            1_000_276_000,
         Physical_Device_Shader_Integer_Dot_Product_Features_Type =>
            1_000_280_000,
         Physical_Device_Shader_Integer_Dot_Product_Properties_Type =>
            1_000_280_001,
         Physical_Device_Texel_Buffer_Alignment_Properties_Type =>
            1_000_281_001,
         Pipeline_Library_Create_Info_Type => 1_000_290_000,
         Present_ID_Type => 1_000_294_000,
         Physical_Device_Present_ID_Features_Type => 1_000_294_001,
         Physical_Device_Private_Data_Features_Type => 1_000_295_000,
         Device_Private_Data_Create_Info_Type => 1_000_295_001,
         Private_Data_Slot_Create_Info_Type => 1_000_295_002,
         Physical_Device_Pipeline_Creation_Cache_Control_Features_Type =>
            1_000_297_000,
         Video_Encode_Info_Type => 1_000_299_000,
         Video_Encode_Rate_Control_Info_Type => 1_000_299_001,
         Video_Encode_Rate_Control_Layer_Info_Type => 1_000_299_002,
         Video_Encode_Capabilities_Type => 1_000_299_003,
         Video_Encode_Usage_Info_Type => 1_000_299_004,
         Query_Pool_Video_Encode_Feedback_Create_Info_Type => 1_000_299_005,
         Physical_Device_Video_Encode_Quality_Level_Info_Type => 1_000_299_006,
         Video_Encode_Quality_Level_Properties_Type => 1_000_299_007,
         Video_Encode_Quality_Level_Info_Type => 1_000_299_008,
         Video_Encode_Session_Parameters_Get_Info_Type => 1_000_299_009,
         Video_Encode_Session_Parameters_Feedback_Info_Type => 1_000_299_010,
         Memory_Barrier_2_Type => 1_000_314_000,
         Buffer_Memory_Barrier_2_Type => 1_000_314_001,
         Image_Memory_Barrier_2_Type => 1_000_314_002,
         Dependency_Info_Type => 1_000_314_003,
         Submit_Info_2_Type => 1_000_314_004,
         Semaphore_Submit_Info_Type => 1_000_314_005,
         Command_Buffer_Submit_Info_Type => 1_000_314_006,
         Physical_Device_Synchronization_2_Features_Type => 1_000_314_007,
         Queue_Family_Checkpoint_Properties_2_Type => 1_000_314_008,
         Checkpoint_Data_2_Type => 1_000_314_009,
         Physical_Device_Fragment_Shader_Barycentric_Properties_Type =>
            1_000_322_000,
         Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type =>
            1_000_323_000,
         Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type =>
            1_000_325_000,
         Physical_Device_Image_Robustness_Features_Type => 1_000_335_000,
         Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type =>
            1_000_336_000,
         Copy_Buffer_Info_2_Type => 1_000_337_000,
         Copy_Image_Info_2_Type => 1_000_337_001,
         Copy_Buffer_To_Image_Info_2_Type => 1_000_337_002,
         Copy_Image_To_Buffer_Info_2_Type => 1_000_337_003,
         Blit_Image_Info_2_Type => 1_000_337_004,
         Resolve_Image_Info_2_Type => 1_000_337_005,
         Buffer_Copy_2_Type => 1_000_337_006,
         Image_Copy_2_Type => 1_000_337_007,
         Image_Blit_2_Type => 1_000_337_008,
         Buffer_Image_Copy_2_Type => 1_000_337_009,
         Image_Resolve_2_Type => 1_000_337_010,
         Subresource_Layout_2_Type => 1_000_338_002,
         Image_Subresource_2_Type => 1_000_338_003,
         Format_Properties_3_Type => 1_000_360_000,
         Physical_Device_Ray_Tracing_Maintenance_1_Features_Type =>
            1_000_386_000,
         Physical_Device_Global_Priority_Query_Features_Type => 1_000_388_000,
         Queue_Family_Global_Priority_Properties_Type => 1_000_388_001,
         Physical_Device_Maintenance_4_Features_Type => 1_000_413_000,
         Physical_Device_Maintenance_4_Properties_Type => 1_000_413_001,
         Device_Buffer_Memory_Requirements_Type => 1_000_413_002,
         Device_Image_Memory_Requirements_Type => 1_000_413_003,
         Physical_Device_Shader_Subgroup_Rotate_Features_Type => 1_000_416_000,
         Physical_Device_Shader_Maximal_Reconvergence_Features_Type =>
            1_000_434_000,
         Physical_Device_Pipeline_Protected_Access_Features_Type =>
            1_000_466_000,
         Physical_Device_Maintenance_5_Features_Type => 1_000_470_000,
         Physical_Device_Maintenance_5_Properties_Type => 1_000_470_001,
         Rendering_Area_Info_Type => 1_000_470_003,
         Device_Image_Subresource_Info_Type => 1_000_470_004,
         Pipeline_Create_Flags_2_Create_Info_Type => 1_000_470_005,
         Buffer_Usage_Flags_2_Create_Info_Type => 1_000_470_006,
         Physical_Device_Ray_Tracing_Position_Fetch_Features_Type =>
            1_000_481_000,
         Physical_Device_Cooperative_Matrix_Features_Type => 1_000_506_000,
         Cooperative_Matrix_Properties_Type => 1_000_506_001,
         Physical_Device_Cooperative_Matrix_Properties_Type => 1_000_506_002,
         Video_Decode_AV1_Capabilities_Type => 1_000_512_000,
         Video_Decode_AV1_Picture_Info_Type => 1_000_512_001,
         Video_Decode_AV1_Profile_Info_Type => 1_000_512_003,
         Video_Decode_AV1_Session_Parameters_Create_Info_Type => 1_000_512_004,
         Video_Decode_AV1_DPB_Slot_Info_Type => 1_000_512_005,
         Physical_Device_Video_Maintenance_1_Features_Type => 1_000_515_000,
         Video_Inline_Query_Info_Type => 1_000_515_001,
         Physical_Device_Vertex_Attribute_Divisor_Properties_Type =>
            1_000_525_000,
         Physical_Device_Shader_Float_Controls_2_Features_Type => 1_000_528_000,
         Physical_Device_Shader_Expect_Assume_Features_Type => 1_000_544_000,
         Physical_Device_Maintenance_6_Features_Type => 1_000_545_000,
         Physical_Device_Maintenance_6_Properties_Type => 1_000_545_001,
         Bind_Memory_Status_Type => 1_000_545_002,
         Bind_Descriptor_Sets_Info_Type => 1_000_545_003,
         Push_Constants_Info_Type => 1_000_545_004,
         Push_Descriptor_Set_Info_Type => 1_000_545_005,
         Push_Descriptor_Set_With_Template_Info_Type => 1_000_545_006,
         Set_Descriptor_Buffer_Offsets_Info_Type => 1_000_545_007,
         Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type => 1_000_545_008);

    subtype Out_Structure_Type is Structure_Type
        with Static_Predicate => Out_Structure_Type in
            Device_Group_Present_Capabilities_Type |
            Physical_Device_Subgroup_Properties_Type |
            Physical_Device_16Bit_Storage_Features_Type |
            Memory_Dedicated_Requirements_Type |
            Physical_Device_Group_Properties_Type |
            Memory_Requirements_2_Type |
            Sparse_Image_Memory_Requirements_2_Type |
            Physical_Device_Features_2_Type |
            Physical_Device_Properties_2_Type |
            Format_Properties_2_Type |
            Image_Format_Properties_2_Type |
            Queue_Family_Properties_2_Type |
            Physical_Device_Memory_Properties_2_Type |
            Sparse_Image_Format_Properties_2_Type |
            Physical_Device_Point_Clipping_Properties_Type |
            Physical_Device_Multiview_Features_Type |
            Physical_Device_Multiview_Properties_Type |
            Physical_Device_Variable_Pointer_Features_Type |
            Physical_Device_Protected_Memory_Features_Type |
            Physical_Device_Protected_Memory_Properties_Type |
            Physical_Device_Sampler_YCbCr_Conversion_Features_Type |
            Sampler_YCbCr_Conversion_Image_Format_Properties_Type |
            External_Image_Format_Properties_Type |
            External_Buffer_Properties_Type |
            Physical_Device_ID_Properties_Type |
            External_Fence_Properties_Type |
            External_Semaphore_Properties_Type |
            Physical_Device_Maintenance_3_Properties_Type |
            Descriptor_Set_Layout_Support_Type |
            Physical_Device_Shader_Draw_Parameter_Features_Type |
            Physical_Device_Vulkan_1_1_Features_Type |
            Physical_Device_Vulkan_1_1_Properties_Type |
            Physical_Device_Vulkan_1_2_Features_Type |
            Physical_Device_Vulkan_1_2_Properties_Type |
            Physical_Device_8Bit_Storage_Features_Type |
            Physical_Device_Driver_Properties_Type |
            Physical_Device_Shader_Atomic_Int64_Features_Type |
            Physical_Device_Shader_Float16_Int8_Features_Type |
            Physical_Device_Float_Controls_Properties_Type |
            Physical_Device_Descriptor_Indexing_Features_Type |
            Physical_Device_Descriptor_Indexing_Properties_Type |
            Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type |
            Physical_Device_Depth_Stencil_Resolve_Properties_Type |
            Physical_Device_Scalar_Block_Layout_Features_Type |
            Physical_Device_Sampler_Filter_Minmax_Properties_Type |
            Physical_Device_Vulkan_Memory_Model_Features_Type |
            Physical_Device_Imageless_Framebuffer_Features_Type |
            Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type |
            Physical_Device_Shader_Subgroup_Extended_Types_Features_Type |
            Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type |
            Attachment_Reference_Stencil_Layout_Type |
            Attachment_Description_Stencil_Layout_Type |
            Physical_Device_Host_Query_Reset_Features_Type |
            Physical_Device_Timeline_Semaphore_Features_Type |
            Physical_Device_Timeline_Semaphore_Properties_Type |
            Physical_Device_Buffer_Device_Address_Features_Type |
            Queue_Family_Query_Result_Status_Properties_Type |
            Queue_Family_Video_Properties_Type |
            Video_Capabilities_Type |
            Video_Format_Properties_Type |
            Video_Session_Memory_Requirements_Type |
            Video_Decode_Capabilities_Type |
            Physical_Device_Vulkan_1_3_Features_Type |
            Physical_Device_Vulkan_1_3_Properties_Type |
            Physical_Device_Shader_Terminate_Invocation_Features_Type |
            Physical_Device_Tool_Properties_Type |
            Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type |
            Physical_Device_Private_Data_Features_Type |
            Physical_Device_Pipeline_Creation_Cache_Control_Features_Type |
            Physical_Device_Synchronization_2_Features_Type |
            Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type |
            Physical_Device_Image_Robustness_Features_Type |
            Physical_Device_Subgroup_Size_Control_Features_Type |
            Physical_Device_Subgroup_Size_Control_Properties_Type |
            Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type |
            Physical_Device_Inline_Uniform_Block_Features_Type |
            Physical_Device_Inline_Uniform_Block_Properties_Type |
            Physical_Device_Texture_Compression_ASTC_HDR_Features_Type |
            Physical_Device_Dynamic_Rendering_Features_Type |
            Physical_Device_Shader_Integer_Dot_Product_Features_Type |
            Physical_Device_Shader_Integer_Dot_Product_Properties_Type |
            Physical_Device_Texel_Buffer_Alignment_Properties_Type |
            Format_Properties_3_Type |
            Physical_Device_Maintenance_4_Features_Type |
            Physical_Device_Maintenance_4_Properties_Type |
            Video_Decode_H264_Capabilities_Type |
            Memory_FD_Properties_Type |
            Physical_Device_Push_Descriptor_Properties_Type |
            Shared_Present_Surface_Capabilities_Type |
            Physical_Device_Performance_Query_Features_Type |
            Physical_Device_Performance_Query_Properties_Type |
            Performance_Counter_Type |
            Performance_Counter_Description_Type |
            Surface_Capabilities_2_Type |
            Surface_Format_2_Type |
            Display_Properties_2_Type |
            Display_Plane_Properties_2_Type |
            Display_Mode_Properties_2_Type |
            Display_Plane_Capabilities_2_Type |
            Physical_Device_Shader_Clock_Features_Type |
            Video_Decode_H265_Capabilities_Type |
            Physical_Device_Global_Priority_Query_Features_Type |
            Queue_Family_Global_Priority_Properties_Type |
            Physical_Device_Fragment_Shading_Rate_Features_Type |
            Physical_Device_Fragment_Shading_Rate_Properties_Type |
            Physical_Device_Fragment_Shading_Rate_Type |
            Physical_Device_Present_Wait_Features_Type |
            Physical_Device_Pipeline_Executable_Properties_Features_Type |
            Pipeline_Executable_Properties_Type |
            Pipeline_Executable_Statistic_Type |
            Pipeline_Executable_Internal_Representation_Type |
            Physical_Device_Present_ID_Features_Type |
            Queue_Family_Checkpoint_Properties_2_Type |
            Checkpoint_Data_2_Type |
            Physical_Device_Fragment_Shader_Barycentric_Features_Type |
            Physical_Device_Fragment_Shader_Barycentric_Properties_Type |
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type |
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type |
            Physical_Device_Ray_Tracing_Maintenance_1_Features_Type |
            Physical_Device_Maintenance_5_Features_Type |
            Physical_Device_Maintenance_5_Properties_Type |
            Image_Subresource_2_Type |
            Subresource_Layout_2_Type |
            Physical_Device_Ray_Tracing_Position_Fetch_Features_Type |
            Cooperative_Matrix_Properties_Type |
            Physical_Device_Cooperative_Matrix_Features_Type |
            Physical_Device_Cooperative_Matrix_Properties_Type |
            Physical_Device_Video_Maintenance_1_Features_Type |
            Physical_Device_Vertex_Attribute_Divisor_Properties_Type |
            Physical_Device_Vertex_Attribute_Divisor_Features_Type |
            Physical_Device_Maintenance_6_Features_Type |
            Physical_Device_Maintenance_6_Properties_Type |
            Physical_Device_Transform_Feedback_Features_Type |
            Physical_Device_Transform_Feedback_Properties_Type |
            Image_View_Address_Properties_Type |
            Texture_LOD_Gather_Format_Properties_Type |
            Physical_Device_Corner_Sampled_Image_Features_Type |
            Physical_Device_ASTC_Decode_Features_Type |
            Physical_Device_Pipeline_Robustness_Features_Type |
            Physical_Device_Pipeline_Robustness_Properties_Type |
            Physical_Device_Conditional_Rendering_Features_Type |
            Surface_Capabilities_2_EXT_Type |
            Physical_Device_Multiview_Per_View_Attributes_Properties_Type |
            Physical_Device_Discard_Rectangle_Properties_Type |
            Physical_Device_Conservative_Rasterization_Properties_Type |
            Physical_Device_Depth_Clip_Enable_Features_Type |
            Physical_Device_Relaxed_Line_Rasterization_Features_Type |
            Video_Encode_H264_Capabilities_Type |
            Video_Encode_H264_Quality_Level_Properties_Type |
            Video_Encode_H264_Session_Parameters_Feedback_Info_Type |
            Video_Encode_H265_Capabilities_Type |
            Video_Encode_H265_Quality_Level_Properties_Type |
            Video_Encode_H265_Session_Parameters_Feedback_Info_Type |
            Physical_Device_Dynamic_Rendering_Local_Read_Features_Type |
            Physical_Device_Shader_Quad_Control_Features_Type |
            Video_Encode_Capabilities_Type |
            Video_Encode_Quality_Level_Properties_Type |
            Video_Encode_Session_Parameters_Feedback_Info_Type |
            Physical_Device_Shader_Maximal_Reconvergence_Features_Type |
            Video_Decode_AV1_Capabilities_Type |
            Physical_Device_Vulkan_1_4_Features_Type |
            Physical_Device_Vulkan_1_4_Properties_Type |
            Physical_Device_Shader_Subgroup_Rotate_Features_Type |
            Physical_Device_Shader_Float_Controls_2_Features_Type |
            Physical_Device_Shader_Expect_Assume_Features_Type |
            Physical_Device_Line_Rasterization_Features_Type |
            Physical_Device_Line_Rasterization_Properties_Type |
            Physical_Device_Index_Type_Uint8_Features_Type |
            Physical_Device_Pipeline_Protected_Access_Features_Type |
            Physical_Device_Host_Image_Copy_Features_Type |
            Physical_Device_Host_Image_Copy_Properties_Type |
            Subresource_Host_Memcpy_Size_Type |
            Host_Image_Copy_Device_Performance_Query_Type;

    subtype In_Structure_Type is Structure_Type
        with Static_Predicate => In_Structure_Type not in Out_Structure_Type;

    type Pipeline_Cache_Header_Version is (One)
        with Convention => C;

    for Pipeline_Cache_Header_Version'Size use 32;

    for Pipeline_Cache_Header_Version use (One => 1);

    type Image_Layout is (Undefined,
                          General,
                          Color_Attachment_Optimal,
                          Depth_Stencil_Attachment_Optimal,
                          Depth_Stencil_Read_Only_Optimal,
                          Shader_Read_Only_Optimal,
                          Transfer_Src_Optimal,
                          Transfer_Dst_Optimal,
                          Preinitialized,
                          Present_Src,
                          Video_Decode_Dst,
                          Video_Decode_Src,
                          Video_Decode_Dpb,
                          Shared_Present,
                          Depth_Read_Only_Stencil_Attachment_Optimal,
                          Depth_Attachment_Stencil_Read_Only_Optimal,
                          Fragment_Shading_Rate_Attachment_Optimal,
                          Fragment_Density_Map_Optimal,
                          Depth_Attachment_Optimal,
                          Depth_Read_Only_Optimal,
                          Stencil_Attachment_Optimal,
                          Stencil_Read_Only_Optimal,
                          Read_Only_Optimal,
                          Attachment_Optimal,
                          Attachment_Feedback_Loop_Optimal)
        with Convention => C;

    for Image_Layout'Size use 32;

    for Image_Layout use
        (Undefined => 0,
         General => 1,
         Color_Attachment_Optimal => 2,
         Depth_Stencil_Attachment_Optimal => 3,
         Depth_Stencil_Read_Only_Optimal => 4,
         Shader_Read_Only_Optimal => 5,
         Transfer_Src_Optimal => 6,
         Transfer_Dst_Optimal => 7,
         Preinitialized => 8,
         Present_Src => 1_000_001_002,
         Video_Decode_Dst => 1_000_024_000,
         Video_Decode_Src => 1_000_024_001,
         Video_Decode_Dpb => 1_000_024_002,
         Shared_Present => 1_000_111_000,
         Depth_Read_Only_Stencil_Attachment_Optimal => 1_000_117_000,
         Depth_Attachment_Stencil_Read_Only_Optimal => 1_000_117_001,
         Fragment_Shading_Rate_Attachment_Optimal => 1_000_164_003,
         Fragment_Density_Map_Optimal => 1_000_218_000,
         Depth_Attachment_Optimal => 1_000_241_000,
         Depth_Read_Only_Optimal => 1_000_241_001,
         Stencil_Attachment_Optimal => 1_000_241_002,
         Stencil_Read_Only_Optimal => 1_000_241_003,
         Read_Only_Optimal => 1_000_314_000,
         Attachment_Optimal => 1_000_314_001,
         Attachment_Feedback_Loop_Optimal => 1_000_339_000);

    type Object_Type is (Unknown_Object_Type,
                         Instance_Object_Type,
                         Physical_Device_Object_Type,
                         Device_Object_Type,
                         Queue_Object_Type,
                         Semaphore_Object_Type,
                         Command_Buffer_Object_Type,
                         Fence_Object_Type,
                         Device_Memory_Object_Type,
                         Buffer_Object_Type,
                         Image_Object_Type,
                         Event_Object_Type,
                         Query_Pool_Object_Type,
                         Buffer_View_Object_Type,
                         Image_View_Object_Type,
                         Shader_Module_Object_Type,
                         Pipeline_Cache_Object_Type,
                         Pipeline_Layout_Object_Type,
                         Render_Pass_Object_Type,
                         Pipeline_Object_Type,
                         Descriptor_Set_Layout_Object_Type,
                         Sampler_Object_Type,
                         Descriptor_Pool_Object_Type,
                         Descriptor_Set_Object_Type,
                         Framebuffer_Object_Type,
                         Command_Pool_Object_Type,
                         Surface_Object_Type,
                         Swapchain_Object_Type,
                         Display_Object_Type,
                         Display_Mode_Object_Type,
                         Video_Session_Object_Type,
                         Video_Session_Parameters_Object_Type,
                         Cu_Module_Object_Type,
                         Cu_Function_Object_Type,
                         Descriptor_Update_Template_Object_Type,
                         Debug_Utils_Messenger_Object_Type,
                         Acceleration_Structure_KHR_Object_Type,
                         Sampler_YCbCr_Conversion_Object_Type,
                         Validation_Cache_Object_Type,
                         Acceleration_Structure_NV_Object_Type,
                         Performance_Configuration_Object_Type,
                         Deferred_Operation_Object_Type,
                         Indirect_Commands_Layout_Object_Type,
                         Private_Data_Slot_Object_Type,
                         Buffer_Collection_Object_Type,
                         Micromap_Object_Type,
                         Optical_Flow_Session_Object_Type,
                         Shader_Object_Type)
        with Convention => C;

    for Object_Type'Size use 32;

    for Object_Type use 
        (Unknown_Object_Type => 0,
         Instance_Object_Type => 1,
         Physical_Device_Object_Type => 2,
         Device_Object_Type => 3,
         Queue_Object_Type => 4,
         Semaphore_Object_Type => 5,
         Command_Buffer_Object_Type => 6,
         Fence_Object_Type => 7,
         Device_Memory_Object_Type => 8,
         Buffer_Object_Type => 9,
         Image_Object_Type => 10,
         Event_Object_Type => 11,
         Query_Pool_Object_Type => 12,
         Buffer_View_Object_Type => 13,
         Image_View_Object_Type => 14,
         Shader_Module_Object_Type => 15,
         Pipeline_Cache_Object_Type => 16,
         Pipeline_Layout_Object_Type => 17,
         Render_Pass_Object_Type => 18,
         Pipeline_Object_Type => 19,
         Descriptor_Set_Layout_Object_Type => 20,
         Sampler_Object_Type => 21,
         Descriptor_Pool_Object_Type => 22,
         Descriptor_Set_Object_Type => 23,
         Framebuffer_Object_Type => 24,
         Command_Pool_Object_Type => 25,
         Surface_Object_Type => 1_000_000_000,
         Swapchain_Object_Type => 1_000_001_000,
         Display_Object_Type => 1_000_002_000,
         Display_Mode_Object_Type => 1_000_002_001,
         Video_Session_Object_Type => 1_000_023_000,
         Video_Session_Parameters_Object_Type => 1_000_023_001,
         Cu_Module_Object_Type => 1_000_029_000,
         Cu_Function_Object_Type => 1_000_029_001,
         Descriptor_Update_Template_Object_Type => 1_000_085_000,
         Debug_Utils_Messenger_Object_Type => 1_000_128_000,
         Acceleration_Structure_KHR_Object_Type => 1_000_150_000,
         Sampler_YCbCr_Conversion_Object_Type => 1_000_156_000,
         Validation_Cache_Object_Type => 1_000_160_000,
         Acceleration_Structure_NV_Object_Type => 1_000_165_000,
         Performance_Configuration_Object_Type => 1_000_210_000,
         Deferred_Operation_Object_Type => 1_000_268_000,
         Indirect_Commands_Layout_Object_Type => 1_000_277_000,
         Private_Data_Slot_Object_Type => 1_000_295_000,
         Buffer_Collection_Object_Type => 1_000_366_000,
         Micromap_Object_Type => 1_000_396_000,
         Optical_Flow_Session_Object_Type => 1_000_464_000,
         Shader_Object_Type => 1_000_482_000);

    type Vendor_ID is (VIV,
                       VSI,
                       Kazan,
                       Codeplay,
                       Mesa,
                       POCL,
                       Mobileye)
        with Convention => C;

    for Vendor_ID'Size use 32;

    for Vendor_ID use (VIV => 16#10001#,
                       VSI => 16#10002#,
                       Kazan => 16#10003#,
                       Codeplay => 16#10004#,
                       Mesa => 16#10005#,
                       POCL => 16#10006#,
                       Mobileye => 16#10007#);

    type System_Allocation_Scope is (Scope_Command,
                                     Scope_Object,
                                     Scope_Cache,
                                     Scope_Device,
                                     Scope_Instance)
        with Convention => C;

    for System_Allocation_Scope'Size use 32;

    for System_Allocation_Scope use (Scope_Command => 0,
                                     Scope_Object => 1,
                                     Scope_Cache => 2,
                                     Scope_Device => 3,
                                     Scope_Instance => 4);

    type Internal_Allocation_Type is (Executable)
        with Convention => C;

    for Internal_Allocation_Type'Size use 32;

    for Internal_Allocation_Type use (Executable => 0);

    type Format is (Undefined,
                    R4G4_UNorm_Pack8,
                    R4G4B4A4_UNorm_Pack16,
                    B4G4R4A4_UNorm_Pack16,
                    R5G5B5_UNorm_Pack16,
                    B5G5R5_UNorm_Pack16,
                    R5G5B5A1_UNorm_Pack16,
                    B5G5R5A1_UNorm_Pack16,
                    A1R5G5B5_UNorm_Pack16,
                    R8_UNorm,
                    R8_SNorm,
                    R8_UScaled,
                    R8_SScaled,
                    R8_UInt,
                    R8_SInt,
                    R8_SRGB,
                    R8G8_UNorm,
                    R8G8_SNorm,
                    R8G8_UScaled,
                    R8G8_SScaled,
                    R8G8_UInt,
                    R8G8_SInt,
                    R8G8_SRGB,
                    R8G8B8_UNorm,
                    R8G8B8_SNorm,
                    R8G8B8_UScaled,
                    R8G8B8_SScaled,
                    R8G8B8_UInt,
                    R8G8B8_SInt,
                    R8G8B8_SRGB,
                    B8G8R8_UNorm,
                    B8G8R8_SNorm,
                    B8G8R8_UScaled,
                    B8G8R8_SScaled,
                    B8G8R8_UInt,
                    B8G8R8_SInt,
                    B8G8R8_SRGB,
                    R8G8B8A8_UNorm,
                    R8G8B8A8_SNorm,
                    R8G8B8A8_UScaled,
                    R8G8B8A8_SScaled,
                    R8G8B8A8_UInt,
                    R8G8B8A8_SInt,
                    R8G8B8A8_SRGB,
                    B8G8R8A8_UNorm,
                    B8G8R8A8_SNorm,
                    B8G8R8A8_UScaled,
                    B8G8R8A8_SScaled,
                    B8G8R8A8_UInt,
                    B8G8R8A8_SInt,
                    B8G8R8A8_SRGB,
                    A8B8G8R8_UNorm_Pack32,
                    A8B8G8R8_SNorm_Pack32,
                    A8B8G8R8_UScaled_Pack32,
                    A8B8G8R8_SScaled_Pack32,
                    A8B8G8R8_UInt_Pack32,
                    A8B8G8R8_SInt_Pack32,
                    A8B8G8R8_SRGB_Pack32,
                    A2R10G10B10_UNorm_Pack32,
                    A2R10G10B10_SNorm_Pack32,
                    A2R10G10B10_UScaled_Pack32,
                    A2R10G10B10_SScaled_Pack32,
                    A2R10G10B10_UInt_Pack32,
                    A2R10G10B10_SInt_Pack32,
                    A2B10G10R10_UNorm_Pack32,
                    A2B10G10R10_SNorm_Pack32,
                    A2B10G10R10_UScaled_Pack32,
                    A2B10G10R10_SScaled_Pack32,
                    A2B10G10R10_UInt_Pack32,
                    A2B10G10R10_SInt_Pack32,
                    R16_UNorm,
                    R16_SNorm,
                    R16_UScaled,
                    S16_UScaled,
                    R16_UInt,
                    R16_SInt,
                    R16_SFloat,
                    R16G16_UNorm,
                    R16G16_SNorm,
                    R16G16_UScaled,
                    S16G16_UScaled,
                    R16G16_UInt,
                    R16G16_SInt,
                    R16G16_SFloat,
                    R16G16B16_UNorm,
                    R16G16B16_SNorm,
                    R16G16B16_UScaled,
                    S16G16B16_UScaled,
                    R16G16B16_UInt,
                    R16G16B16_SInt,
                    R16G16B16_SFloat,
                    R16G16B16A16_UNorm,
                    R16G16B16A16_SNorm,
                    R16G16B16A16_UScaled,
                    S16G16B16A16_UScaled,
                    R16G16B16A16_UInt,
                    R16G16B16A16_SInt,
                    R16G16B16A16_SFloat,
                    R32_UInt,
                    R32_SInt,
                    R32_SFloat,
                    R32G32_UInt,
                    R32G32_SInt,
                    R32G32_SFloat,
                    R32G32B32_UInt,
                    R32G32B32_SInt,
                    R32G32B32_SFloat,
                    R32G32B32A32_UInt,
                    R32G32B32A32_SInt,
                    R32G32B32A32_SFloat,
                    R64_UInt,
                    R64_SInt,
                    R64_SFloat,
                    R64G64_UInt,
                    R64G64_SInt,
                    R64G64_SFloat,
                    R64G64B64_UInt,
                    R64G64B64_SInt,
                    R64G64B64_SFloat,
                    R64G64B64A64_UInt,
                    R64G64B64A64_SInt,
                    R64G64B64A64_SFloat,
                    B10G11R11_UFloat_Pack32,
                    E5B9G9R9_UFloat_Pack32,
                    D16_UNorm,
                    X8_D24_UNorm_Pack32,
                    D32_SFloat,
                    S8_UInt,
                    D16_UNorm_S8_UInt,
                    D24_UNorm_S8_Uint,
                    D32_SFloat_S8_UInt,
                    BC1_RGB_UNorm_Block,
                    BC1_RGB_SRGB_Block,
                    BC1_RGBA_UNorm_Block,
                    BC1_RGBA_SRGB_Block,
                    BC2_RGB_UNorm_Block,
                    BC2_RGB_SRGB_Block,
                    BC3_RGB_UNorm_Block,
                    BC3_RGB_SRGB_Block,
                    BC4_UNorm_Block,
                    BC4_SNorm_Block,
                    BC5_UNorm_Block,
                    BC5_SNorm_Block,
                    BC6H_UFloat_Block,
                    BC6H_SFloat_Block,
                    BC7_UNorm_Block,
                    BC7_SRGB_Block,
                    ETC2_R8G8B8_UNorm_Block,
                    ETC2_R8G8B8_SRGB_Block,
                    ETC2_R8G8B8A1_UNorm_Block,
                    ETC2_R8G8B8A1_SRGB_Block,
                    ETC2_R8G8B8A8_UNorm_Block,
                    ETC2_R8G8B8A8_SRGB_Block,
                    EAC_R11_UNorm_Block,
                    EAC_R11_SNorm_Block,
                    EAC_R11G11_UNorm_Block,
                    EAC_R11G11_SNorm_Block,
                    ASTC_4x4_UNorm_Block,
                    ASTC_4x4_SRGB_Block,
                    ASTC_5x4_UNorm_Block,
                    ASTC_5x4_SRGB_Block,
                    ASTC_5x5_UNorm_Block,
                    ASTC_5x5_SRGB_Block,
                    ASTC_6x5_UNorm_Block,
                    ASTC_6x5_SRGB_Block,
                    ASTC_6x6_UNorm_Block,
                    ASTC_6x6_SRGB_Block,
                    ASTC_8x5_UNorm_Block,
                    ASTC_8x5_SRGB_Block,
                    ASTC_8x6_UNorm_Block,
                    ASTC_8x6_SRGB_Block,
                    ASTC_8x8_UNorm_Block,
                    ASTC_8x8_SRGB_Block,
                    ASTC_10x5_UNorm_Block,
                    ASTC_10x5_SRGB_Block,
                    ASTC_10x6_UNorm_Block,
                    ASTC_10x6_SRGB_Block,
                    ASTC_10x8_UNorm_Block,
                    ASTC_10x8_SRGB_Block,
                    ASTC_10x10_UNorm_Block,
                    ASTC_10x10_SRGB_Block,
                    ASTC_12x10_UNorm_Block,
                    ASTC_12x10_SRGB_Block,
                    ASTC_12x12_UNorm_Block,
                    ASTC_12x12_SRGB_Block,
                    PVRTC1_2BPP_UNorm_Block_Img,
                    PVRTC1_4BPP_UNorm_Block_Img,
                    PVRTC2_2BPP_UNorm_Block_Img,
                    PVRTC2_4BPP_UNorm_Block_Img,
                    PVRTC1_2BPP_SRGB_Block_Img,
                    PVRTC1_4BPP_SRGB_Block_Img,
                    PVRTC2_2BPP_SRGB_Block_Img,
                    PVRTC2_4BPP_SRGB_Block_Img,
                    ASTC_4x4_SFloat_Block,
                    ASTC_5x4_SFloat_Block,
                    ASTC_5x5_SFloat_Block,
                    ASTC_6x5_SFloat_Block,
                    ASTC_6x6_SFloat_Block,
                    ASTC_8x5_SFloat_Block,
                    ASTC_8x6_SFloat_Block,
                    ASTC_8x8_SFloat_Block,
                    ASTC_10x5_SFloat_Block, 
                    ASTC_10x6_SFloat_Block,
                    ASTC_10x8_SFloat_Block, 
                    ASTC_10x10_SFloat_Block,
                    ASTC_12x10_SFloat_Block,
                    ASTC_12x12_SFloat_Block,
                    G8B8G8R8_422_UNorm,
                    B8G8R8G8_422_UNorm,
                    G8_B8_R8_3Plane_420_UNorm,
                    G8_B8R8_2Plane_420_UNorm,
                    G8_B8_R8_3Plane_422_UNorm,
                    G8_B8R8_2Plane_422_UNorm,
                    G8_B8_R8_3Plane_444_UNorm,
                    R10X6_UNorm_Pack16,
                    R10X6G10X6_UNorm_2Pack16,
                    R10X6G10X6B10X6A10X6_UNorm_4Pack16,
                    G10X6B10X6G10X6R10X6_422_UNorm_4Pack16,
                    B10X6G10X6R10X6G10X6_422_UNorm_4Pack16,
                    G10X6_B10X6_R10X6_3Plane_420_UNorm_3Pack16,
                    G10X6_B10X6R10X6_2Plane_420_UNorm_3Pack16,
                    G10X6_B10X6_R10X6_3Plane_422_UNorm_3Pack16,
                    G10X6_B10X6R10X6_2Plane_422_UNorm_3Pack16,
                    G10X6_B10X6_R10X6_3Plane_444_UNorm_3Pack16,
                    R12X4_UNorm_Pack16,
                    R12X4G12X4_UNorm_2Pack16,
                    R12X4G12X4B12X4A12X4_UNorm_4Pack16,
                    G12X4B12X4G12X4R12X4_422_UNorm_4Pack16,
                    B12X4G12X4R12X4G12X4_422_UNorm_4Pack16,
                    G12X4_B12X4_R12X4_3Plane_420_UNorm_3Pack16,
                    G12X4_B12X4R12X4_2Plane_420_UNorm_3Pack16,
                    G12X4_B12X4_R12X4_3Plane_422_UNorm_3Pack16,
                    G12X4_B12X4R12X4_2Plane_422_UNorm_3Pack16,
                    G12X4_B12X4_R12X4_3Plane_444_UNorm_3Pack16,
                    G16B16G16R16_422_UNorm,
                    B16G16R16G16_422_UNorm,
                    G16_B16_R16_3Plane_420_UNorm,
                    G16_B16R16_2Plane_420_UNorm,
                    G16_B16_R16_3Plane_422_UNorm,
                    G16_B16R16_2Plane_422_UNorm,
                    G16_B16_R16_3Plane_444_UNorm,
                    G8_B8R8_2Plane_444_UNorm,
                    G10X6_B10X6R10X6_2Plane_444_UNorm_3Pack16,
                    G12X4_B12X4R12X4_2Plane_444_UNorm_3Pack16,
                    G16_B16R16_2Plane_444_UNorm,
                    A4R4G4B4_UNorm_Pack16,
                    A4B4G4R4_UNorm_Pack16,
                    R16G16_S10_5)
        with Convention => C;

    for Format'Size use 32;

    for Format use (Undefined => 0,
                    R4G4_UNorm_Pack8 => 1,
                    R4G4B4A4_UNorm_Pack16 => 2,
                    B4G4R4A4_UNorm_Pack16 => 3,
                    R5G5B5_UNorm_Pack16 => 4,
                    B5G5R5_UNorm_Pack16 => 5,
                    R5G5B5A1_UNorm_Pack16 => 6,
                    B5G5R5A1_UNorm_Pack16 => 7,
                    A1R5G5B5_UNorm_Pack16 => 8,
                    R8_UNorm => 9,
                    R8_SNorm => 10,
                    R8_UScaled => 11,
                    R8_SScaled => 12,
                    R8_UInt => 13,
                    R8_SInt => 14,
                    R8_SRGB => 15,
                    R8G8_UNorm => 16,
                    R8G8_SNorm => 17,
                    R8G8_UScaled => 18,
                    R8G8_SScaled => 19,
                    R8G8_UInt => 20,
                    R8G8_SInt => 21,
                    R8G8_SRGB => 22,
                    R8G8B8_UNorm => 23,
                    R8G8B8_SNorm => 24,
                    R8G8B8_UScaled => 25,
                    R8G8B8_SScaled => 26,
                    R8G8B8_UInt => 27,
                    R8G8B8_SInt => 28,
                    R8G8B8_SRGB => 29,
                    B8G8R8_UNorm => 30,
                    B8G8R8_SNorm => 31,
                    B8G8R8_UScaled => 32,
                    B8G8R8_SScaled => 33,
                    B8G8R8_UInt => 34,
                    B8G8R8_SInt => 35,
                    B8G8R8_SRGB => 36,
                    R8G8B8A8_UNorm => 37,
                    R8G8B8A8_SNorm => 38,
                    R8G8B8A8_UScaled => 39,
                    R8G8B8A8_SScaled => 40,
                    R8G8B8A8_UInt => 41,
                    R8G8B8A8_SInt => 42,
                    R8G8B8A8_SRGB => 43,
                    B8G8R8A8_UNorm => 44,
                    B8G8R8A8_SNorm => 45,
                    B8G8R8A8_UScaled => 46,
                    B8G8R8A8_SScaled => 47,
                    B8G8R8A8_UInt => 48,
                    B8G8R8A8_SInt => 49,
                    B8G8R8A8_SRGB => 50,
                    A8B8G8R8_UNorm_Pack32 => 51,
                    A8B8G8R8_SNorm_Pack32 => 52,
                    A8B8G8R8_UScaled_Pack32 => 53,
                    A8B8G8R8_SScaled_Pack32 => 54,
                    A8B8G8R8_UInt_Pack32 => 55,
                    A8B8G8R8_SInt_Pack32 => 56,
                    A8B8G8R8_SRGB_Pack32 => 57,
                    A2R10G10B10_UNorm_Pack32 => 58,
                    A2R10G10B10_SNorm_Pack32 => 59,
                    A2R10G10B10_UScaled_Pack32 => 60,
                    A2R10G10B10_SScaled_Pack32 => 61,
                    A2R10G10B10_UInt_Pack32 => 62,
                    A2R10G10B10_SInt_Pack32 => 63,
                    A2B10G10R10_UNorm_Pack32 => 64,
                    A2B10G10R10_SNorm_Pack32 => 65,
                    A2B10G10R10_UScaled_Pack32 => 66,
                    A2B10G10R10_SScaled_Pack32 => 67,
                    A2B10G10R10_UInt_Pack32 => 68,
                    A2B10G10R10_SInt_Pack32 => 69,
                    R16_UNorm => 70,
                    R16_SNorm => 71,
                    R16_UScaled => 72,
                    S16_UScaled => 73,
                    R16_UInt => 74,
                    R16_SInt => 75,
                    R16_SFloat => 76,
                    R16G16_UNorm => 77,
                    R16G16_SNorm => 78,
                    R16G16_UScaled => 79,
                    S16G16_UScaled => 80,
                    R16G16_UInt => 81,
                    R16G16_SInt => 82,
                    R16G16_SFloat => 83,
                    R16G16B16_UNorm => 84,
                    R16G16B16_SNorm => 85,
                    R16G16B16_UScaled => 86,
                    S16G16B16_UScaled => 87,
                    R16G16B16_UInt => 88,
                    R16G16B16_SInt => 89,
                    R16G16B16_SFloat => 90,
                    R16G16B16A16_UNorm => 91,
                    R16G16B16A16_SNorm => 92,
                    R16G16B16A16_UScaled => 93,
                    S16G16B16A16_UScaled => 94,
                    R16G16B16A16_UInt => 95,
                    R16G16B16A16_SInt => 96,
                    R16G16B16A16_SFloat => 97,
                    R32_UInt => 98,
                    R32_SInt => 99,
                    R32_SFloat => 100,
                    R32G32_UInt => 101,
                    R32G32_SInt => 102,
                    R32G32_SFloat => 103,
                    R32G32B32_UInt => 104,
                    R32G32B32_SInt => 105,
                    R32G32B32_SFloat => 106,
                    R32G32B32A32_UInt => 107,
                    R32G32B32A32_SInt => 108,
                    R32G32B32A32_SFloat => 109,
                    R64_UInt => 110,
                    R64_SInt => 111,
                    R64_SFloat => 112,
                    R64G64_UInt => 113,
                    R64G64_SInt => 114,
                    R64G64_SFloat => 115,
                    R64G64B64_UInt => 116,
                    R64G64B64_SInt => 117,
                    R64G64B64_SFloat => 118,
                    R64G64B64A64_UInt => 119,
                    R64G64B64A64_SInt => 120,
                    R64G64B64A64_SFloat => 121,
                    B10G11R11_UFloat_Pack32 => 122,
                    E5B9G9R9_UFloat_Pack32 => 123,
                    D16_UNorm => 124,
                    X8_D24_UNorm_Pack32 => 125,
                    D32_SFloat => 126,
                    S8_UInt => 127,
                    D16_UNorm_S8_UInt => 128,
                    D24_UNorm_S8_Uint => 129,
                    D32_SFloat_S8_UInt => 130,
                    BC1_RGB_UNorm_Block => 131,
                    BC1_RGB_SRGB_Block => 132,
                    BC1_RGBA_UNorm_Block => 133,
                    BC1_RGBA_SRGB_Block => 134,
                    BC2_RGB_UNorm_Block => 135,
                    BC2_RGB_SRGB_Block => 136,
                    BC3_RGB_UNorm_Block => 137,
                    BC3_RGB_SRGB_Block => 138,
                    BC4_UNorm_Block => 139,
                    BC4_SNorm_Block => 140,
                    BC5_UNorm_Block => 141,
                    BC5_SNorm_Block => 142,
                    BC6H_UFloat_Block => 143,
                    BC6H_SFloat_Block => 144,
                    BC7_UNorm_Block => 145,
                    BC7_SRGB_Block => 146,
                    ETC2_R8G8B8_UNorm_Block => 147,
                    ETC2_R8G8B8_SRGB_Block => 148,
                    ETC2_R8G8B8A1_UNorm_Block => 149,
                    ETC2_R8G8B8A1_SRGB_Block => 150,
                    ETC2_R8G8B8A8_UNorm_Block => 151,
                    ETC2_R8G8B8A8_SRGB_Block => 152,
                    EAC_R11_UNorm_Block => 153,
                    EAC_R11_SNorm_Block => 154,
                    EAC_R11G11_UNorm_Block => 155,
                    EAC_R11G11_SNorm_Block => 156,
                    ASTC_4x4_UNorm_Block => 157,
                    ASTC_4x4_SRGB_Block => 158,
                    ASTC_5x4_UNorm_Block => 159,
                    ASTC_5x4_SRGB_Block => 160,
                    ASTC_5x5_UNorm_Block => 161,
                    ASTC_5x5_SRGB_Block => 162,
                    ASTC_6x5_UNorm_Block => 163,
                    ASTC_6x5_SRGB_Block => 164,
                    ASTC_6x6_UNorm_Block => 165,
                    ASTC_6x6_SRGB_Block => 166,
                    ASTC_8x5_UNorm_Block => 167,
                    ASTC_8x5_SRGB_Block => 168,
                    ASTC_8x6_UNorm_Block => 169,
                    ASTC_8x6_SRGB_Block => 170,
                    ASTC_8x8_UNorm_Block => 171,
                    ASTC_8x8_SRGB_Block => 172,
                    ASTC_10x5_UNorm_Block => 173,
                    ASTC_10x5_SRGB_Block => 174,
                    ASTC_10x6_UNorm_Block => 175,
                    ASTC_10x6_SRGB_Block => 176,
                    ASTC_10x8_UNorm_Block => 177,
                    ASTC_10x8_SRGB_Block => 178,
                    ASTC_10x10_UNorm_Block => 179,
                    ASTC_10x10_SRGB_Block => 180,
                    ASTC_12x10_UNorm_Block => 181,
                    ASTC_12x10_SRGB_Block => 182,
                    ASTC_12x12_UNorm_Block => 183,
                    ASTC_12x12_SRGB_Block => 184,
                    PVRTC1_2BPP_UNorm_Block_Img => 1_000_054_000,
                    PVRTC1_4BPP_UNorm_Block_Img => 1_000_054_001,
                    PVRTC2_2BPP_UNorm_Block_Img => 1_000_054_002,
                    PVRTC2_4BPP_UNorm_Block_Img => 1_000_054_003,
                    PVRTC1_2BPP_SRGB_Block_Img => 1_000_054_004,
                    PVRTC1_4BPP_SRGB_Block_Img => 1_000_054_005,
                    PVRTC2_2BPP_SRGB_Block_Img => 1_000_054_006,
                    PVRTC2_4BPP_SRGB_Block_Img => 1_000_054_007,
                    ASTC_4x4_SFloat_Block => 1_000_066_000,
                    ASTC_5x4_SFloat_Block => 1_000_066_001,
                    ASTC_5x5_SFloat_Block => 1_000_066_002,
                    ASTC_6x5_SFloat_Block => 1_000_066_003,
                    ASTC_6x6_SFloat_Block => 1_000_066_004,
                    ASTC_8x5_SFloat_Block => 1_000_066_005,
                    ASTC_8x6_SFloat_Block => 1_000_066_006,
                    ASTC_8x8_SFloat_Block => 1_000_066_007,
                    ASTC_10x5_SFloat_Block => 1_000_066_008,
                    ASTC_10x6_SFloat_Block => 1_000_066_009,
                    ASTC_10x8_SFloat_Block => 1_000_066_010,
                    ASTC_10x10_SFloat_Block => 1_000_066_011,
                    ASTC_12x10_SFloat_Block => 1_000_066_012,
                    ASTC_12x12_SFloat_Block => 1_000_066_013,
                    G8B8G8R8_422_UNorm => 1_000_156_000,
                    B8G8R8G8_422_UNorm => 1_000_156_001,
                    G8_B8_R8_3Plane_420_UNorm => 1_000_156_002,
                    G8_B8R8_2Plane_420_UNorm => 1_000_156_003,
                    G8_B8_R8_3Plane_422_UNorm => 1_000_156_004,
                    G8_B8R8_2Plane_422_UNorm => 1_000_156_005,
                    G8_B8_R8_3Plane_444_UNorm => 1_000_156_006,
                    R10X6_UNorm_Pack16 => 1_000_156_007,
                    R10X6G10X6_UNorm_2Pack16 => 1_000_156_008,
                    R10X6G10X6B10X6A10X6_UNorm_4Pack16 => 1_000_156_009,
                    G10X6B10X6G10X6R10X6_422_UNorm_4Pack16 => 1_000_156_010,
                    B10X6G10X6R10X6G10X6_422_UNorm_4Pack16 => 1_000_156_011,
                    G10X6_B10X6_R10X6_3Plane_420_UNorm_3Pack16 => 1_000_156_012,
                    G10X6_B10X6R10X6_2Plane_420_UNorm_3Pack16 => 1_000_156_013,
                    G10X6_B10X6_R10X6_3Plane_422_UNorm_3Pack16 => 1_000_156_014,
                    G10X6_B10X6R10X6_2Plane_422_UNorm_3Pack16 => 1_000_156_015,
                    G10X6_B10X6_R10X6_3Plane_444_UNorm_3Pack16 => 1_000_156_016,
                    R12X4_UNorm_Pack16 => 1_000_156_017,
                    R12X4G12X4_UNorm_2Pack16 => 1_000_156_018,
                    R12X4G12X4B12X4A12X4_UNorm_4Pack16 => 1_000_156_019,
                    G12X4B12X4G12X4R12X4_422_UNorm_4Pack16 => 1_000_156_020,
                    B12X4G12X4R12X4G12X4_422_UNorm_4Pack16 => 1_000_156_021,
                    G12X4_B12X4_R12X4_3Plane_420_UNorm_3Pack16 => 1_000_156_022,
                    G12X4_B12X4R12X4_2Plane_420_UNorm_3Pack16 => 1_000_156_023,
                    G12X4_B12X4_R12X4_3Plane_422_UNorm_3Pack16 => 1_000_156_024,
                    G12X4_B12X4R12X4_2Plane_422_UNorm_3Pack16 => 1_000_156_025,
                    G12X4_B12X4_R12X4_3Plane_444_UNorm_3Pack16 => 1_000_156_026,
                    G16B16G16R16_422_UNorm => 1_000_156_027,
                    B16G16R16G16_422_UNorm => 1_000_156_028,
                    G16_B16_R16_3Plane_420_UNorm => 1_000_156_029,
                    G16_B16R16_2Plane_420_UNorm => 1_000_156_030,
                    G16_B16_R16_3Plane_422_UNorm => 1_000_156_031,
                    G16_B16R16_2Plane_422_UNorm => 1_000_156_032,
                    G16_B16_R16_3Plane_444_UNorm => 1_000_156_033,
                    G8_B8R8_2Plane_444_UNorm => 1_000_330_000,
                    G10X6_B10X6R10X6_2Plane_444_UNorm_3Pack16 => 1_000_330_001,
                    G12X4_B12X4R12X4_2Plane_444_UNorm_3Pack16 => 1_000_330_002,
                    G16_B16R16_2Plane_444_UNorm => 1_000_330_003,
                    A4R4G4B4_UNorm_Pack16 => 1_000_340_000,
                    A4B4G4R4_UNorm_Pack16 => 1_000_340_001,
                    R16G16_S10_5 => 1_000_464_000);

    type Image_Tiling is (Optimal,
                          Linear,
                          DRM_Format_Modifier)
        with Convention => C;

    for Image_Tiling'Size use 32;

    for Image_Tiling use (Optimal => 0,
                          Linear => 1,
                          DRM_Format_Modifier => 1_000_158_000);

    type Image_Type is (Type_1D,
                        Type_2D,
                        Type_3D)
        with Convention => C;

    for Image_Type'Size use 32;

    for Image_Type use (Type_1D => 0,
                        Type_2D => 1,
                        Type_3D => 2);

    type Physical_Device_Type is (Other,
                                  Integrated_GPU,
                                  Discrete_GPU,
                                  Virtual_GPU,
                                  CPU)
        with Convention => C;

    for Physical_Device_Type'Size use 32;

    for Physical_Device_Type use (Other => 0,
                                  Integrated_GPU => 1,
                                  Discrete_GPU => 2,
                                  Virtual_GPU => 3,
                                  CPU => 4);

    type Query_Type is
        (Occlusion,
         Pipeline_Statistics,
         Timestamp,
         Result_Status_Only,
         Transform_Feedback_Stream,
         Performance_Query_KHR,
         Acceleration_Structure_Compacted_Size_KHR,
         Acceleration_Structure_Serialization_Size,
         Acceleration_Structure_Compacted_Size_NV,
         Performance_Query_INTEL,
         Mesh_Primitives_Generated,
         Primitives_Generated,
         Acceleration_Structure_Serialization_Bottom_Level_Pointers,
         Acceleration_Structure_Size,
         Micromap_Serialization_Size,
         Micromap_Compacted_Size)
        with Convention => C;

    for Query_Type'Size use 32;

    for Query_Type use
        (Occlusion => 0,
         Pipeline_Statistics => 1,
         Timestamp => 2,
         Result_Status_Only => 1_000_023_000,
         Transform_Feedback_Stream => 1_000_028_004,
         Performance_Query_KHR => 1_000_116_000,
         Acceleration_Structure_Compacted_Size_KHR => 1_000_150_000,
         Acceleration_Structure_Serialization_Size => 1_000_150_001,
         Acceleration_Structure_Compacted_Size_NV => 1_000_165_000,
         Performance_Query_INTEL => 1_000_210_000,
         Mesh_Primitives_Generated => 1_000_328_000,
         Primitives_Generated => 1_000_382_000,
         Acceleration_Structure_Serialization_Bottom_Level_Pointers
             => 1_000_386_000,
         Acceleration_Structure_Size => 1_000_386_001,
         Micromap_Serialization_Size => 1_000_396_000,
         Micromap_Compacted_Size => 1_000_396_001);
    
    type Sharing_Mode is (Exclusive,
                          Concurrent)
        with Convention => C;

    for Sharing_Mode'Size use 32;

    for Sharing_Mode use (Exclusive => 0,
                          Concurrent => 1);
  
    type Component_Swizzle is (Identity,
                               Zero,
                               One,
                               R,
                               G,
                               B,
                               A)
        with Convention => C;

    for Component_Swizzle'Size use 32;

    for Component_Swizzle use (Identity => 0,
                               Zero => 1,
                               One => 2,
                               R => 3,
                               G => 4,
                               B => 5,
                               A => 6);

    type Image_View_Type is (Type_1D,
                             Type_2D,
                             Type_3D,
                             Cube,
                             Type_1D_Array,
                             Type_2D_Array,
                             Cube_Array)
        with Convention => C;

    for Image_View_Type'Size use 32;

    for Image_View_Type use (Type_1D => 0,
                             Type_2D => 1,
                             Type_3D => 2,
                             Cube => 3,
                             Type_1D_Array => 4,
                             Type_2D_Array => 5,
                             Cube_Array => 6);
 
    type Blend_Factor is (Zero,
                          One,
                          Src_Color,
                          One_Minus_Src_Color,
                          Dst_Color,
                          One_Minus_Dst_Color,
                          Src_Alpha,
                          One_Minus_Src_Alpha,
                          Dst_Alpha,
                          One_Minus_Dst_Alpha,
                          Constant_Color,
                          One_Minus_Constant_Color,
                          Constant_Alpha,
                          One_Minus_Constant_Alpha,
                          Src_Alpha_Saturate,
                          Src1_Color,
                          One_Minus_Src1_Color,
                          Src1_Alpha,
                          One_Minus_Src1_Alpha)
        with Convention => C;

    for Blend_Factor'Size use 32;

    for Blend_Factor use (Zero => 0,
                          One => 1,
                          Src_Color => 2,
                          One_Minus_Src_Color => 3,
                          Dst_Color => 4,
                          One_Minus_Dst_Color => 5,
                          Src_Alpha => 6,
                          One_Minus_Src_Alpha => 7,
                          Dst_Alpha => 8,
                          One_Minus_Dst_Alpha => 9,
                          Constant_Color => 10,
                          One_Minus_Constant_Color => 11,
                          Constant_Alpha => 12,
                          One_Minus_Constant_Alpha => 13,
                          Src_Alpha_Saturate => 14,
                          Src1_Color => 15,
                          One_Minus_Src1_Color => 16,
                          Src1_Alpha => 17,
                          One_Minus_Src1_Alpha => 18);

    type Blend_Op is (Add,
                      Subtract,
                      Reverse_Subtract,
                      Min,
                      Max,
                      Zero,
                      Src,
                      Dst,
                      Src_Over,
                      Dst_Over,
                      Src_In,
                      Dst_In,
                      Src_Out,
                      Dst_Out,
                      Src_Atop,
                      Dst_Atop,
                      Xor_Op,
                      Multiply,
                      Screen,
                      Overlay,
                      Darken,
                      Lighten,
                      Colordodge,
                      Colorburn,
                      Hardlight,
                      Softlight,
                      Difference,
                      Exclusion,
                      Invert,
                      Invert_RGB,
                      Lineardodge,
                      Linearburn,
                      Vividlight,
                      Linearlight,
                      Pinlight,
                      Hardmix,
                      HSL_Hue,
                      HSL_Saturation,
                      HSL_Color,
                      HSL_Luminosity,
                      Plus,
                      Plus_Clamped,
                      Plus_Clamped_Alpha,
                      Plus_Darker,
                      Minus,
                      Minus_Clamped,
                      Contrast,
                      Invert_OVG,
                      Red,
                      Green,
                      Blue)
        with Convention => C;

    for Blend_Op'Size use 32;

    for Blend_Op use (Add => 0,
                      Subtract => 1,
                      Reverse_Subtract => 2,
                      Min => 3,
                      Max => 4,
                      Zero => 1_000_148_000,
                      Src => 1_000_148_001,
                      Dst => 1_000_148_002,
                      Src_Over => 1_000_148_003,
                      Dst_Over => 1_000_148_004,
                      Src_In => 1_000_148_005,
                      Dst_In => 1_000_148_006,
                      Src_Out => 1_000_148_007,
                      Dst_Out => 1_000_148_008,
                      Src_Atop => 1_000_148_009,
                      Dst_Atop => 1_000_148_010,
                      Xor_Op => 1_000_148_011,
                      Multiply => 1_000_148_012,
                      Screen => 1_000_148_013,
                      Overlay => 1_000_148_014,
                      Darken => 1_000_148_015,
                      Lighten => 1_000_148_016,
                      Colordodge => 1_000_148_017,
                      Colorburn => 1_000_148_018,
                      Hardlight => 1_000_148_019,
                      Softlight => 1_000_148_020,
                      Difference => 1_000_148_021,
                      Exclusion => 1_000_148_022,
                      Invert => 1_000_148_023,
                      Invert_RGB => 1_000_148_024,
                      Lineardodge => 1_000_148_025,
                      Linearburn => 1_000_148_026,
                      Vividlight => 1_000_148_027,
                      Linearlight => 1_000_148_028,
                      Pinlight => 1_000_148_029,
                      Hardmix => 1_000_148_030,
                      HSL_Hue => 1_000_148_031,
                      HSL_Saturation => 1_000_148_032,
                      HSL_Color => 1_000_148_033,
                      HSL_Luminosity => 1_000_148_034,
                      Plus => 1_000_148_035,
                      Plus_Clamped => 1_000_148_036,
                      Plus_Clamped_Alpha => 1_000_148_037,
                      Plus_Darker => 1_000_148_038,
                      Minus => 1_000_148_039,
                      Minus_Clamped => 1_000_148_040,
                      Contrast => 1_000_148_041,
                      Invert_OVG => 1_000_148_042,
                      Red => 1_000_148_043,
                      Green => 1_000_148_044,
                      Blue => 1_000_148_045);

    type Compare_Op is (Never,
                        Less,
                        Equal,
                        Less_Or_Equal,
                        Greater,
                        Not_Equal,
                        Greater_Or_Equal,
                        Always)
        with Convention => C;

    for Compare_Op'Size use 32;

    for Compare_Op use (Never => 0,
                        Less => 1,
                        Equal => 2,
                        Less_Or_Equal => 3,
                        Greater => 4,
                        Not_Equal => 5,
                        Greater_Or_Equal => 6,
                        Always => 7);

    type Dynamic_State is (State_Viewport,
                           Scissor,
                           Line_Width,
                           Depth_Bias,
                           Blend_Constants,
                           Depth_Bounds,
                           Stencil_Compare_Mask,
                           Stencil_Write_Mask,
                           Stencil_Reference,
                           State_Viewport_W_Scaling,
                           Discard_Rectangle,
                           Discard_Rectangle_Enable,
                           State_Discard_Rectangle_Mode,
                           Sample_Locations,
                           Viewport_Shading_Rate_Palette,
                           Viewport_Coarse_Sample_Order,
                           Exclusive_Scissor_Enable,
                           Exclusive_Scissor,
                           Fragment_Shading_Rate,
                           Line_Stipple,
                           Cull_Mode,
                           State_Front_Face,
                           State_Primitive_Topology,
                           Viewport_With_Count,
                           Scissor_With_Count,
                           Vertex_Input_Binding_Stride,
                           Depth_Test_Enable,
                           Depth_Write_Enable,
                           Depth_Compare_Op,
                           Depths_Bounds_Test_Enable,
                           Stencil_Test_Enable,
                           State_Stencil_Op,
                           Raytracing_Pipeline_Stack_Size,
                           Vertex_Input,
                           Patch_Control_Points,
                           Rasterizer_Discard_Enable,
                           Depth_Bias_Enable,
                           State_Logic_Op,
                           Primitive_Restart_Enable,
                           Color_Write_Enable,
                           State_Tessellation_Domain_Origin,
                           Depth_Clamp_Enable,
                           State_Polygon_Mode,
                           Rasterization_Samples,
                           State_Sample_Mask,
                           Alpha_To_Converage_Enable,
                           Alpha_To_One_Enable,
                           Logic_Op_Enable,
                           Color_Blend_Enable,
                           Color_Blend_Equation,
                           Color_Write_Mask,
                           Rasterization_Stream,
                           State_Conservative_Rasterization_Mode,
                           Extra_Primitive_Overestimation_Size,
                           Depth_Clip_Enable,
                           Sample_Locations_Enable,
                           Color_Blend_Advanced,
                           Provoking_Vertex_Mode,
                           State_Line_Rasterization_Mode,
                           Line_Stipple_Enable,
                           Depth_Clip_Negative_One_To_One,
                           Viewport_W_Scaling_Enable,
                           State_Viewport_Swizzle,
                           Coverage_To_Color_Enable,
                           Coverage_To_Color_Location,
                           Coverage_Modulation_Mode,
                           Coverage_Modulation_Table_Enable,
                           Coverage_Modulation_Table,
                           Shading_Rate_Image_Enable,
                           Representative_Fragment_Test_Enable,
                           Coverage_Reduction_Mode,
                           Attachment_Feedback_Loop_Enable)
        with Convention => C;

    for Dynamic_State'Size use 32;

    for Dynamic_State use 
        (State_Viewport => 0,
         Scissor => 1,
         Line_Width => 2,
         Depth_Bias => 3,
         Blend_Constants => 4,
         Depth_Bounds => 5,
         Stencil_Compare_Mask => 6,
         Stencil_Write_Mask => 7,
         Stencil_Reference => 8,
         State_Viewport_W_Scaling => 1_000_087_000,
         Discard_Rectangle => 1_000_099_000,
         Discard_Rectangle_Enable => 1_000_099_001,
         State_Discard_Rectangle_Mode => 1_000_099_002,
         Sample_Locations => 1_000_143_000,
         Viewport_Shading_Rate_Palette => 1_000_164_004,
         Viewport_Coarse_Sample_Order => 1_000_164_006,
         Exclusive_Scissor_Enable => 1_000_205_000,
         Exclusive_Scissor => 1_000_205_001,
         Fragment_Shading_Rate => 1_000_226_000,
         Line_Stipple => 1_000_259_000,
         Cull_Mode => 1_000_267_000,
         State_Front_Face => 1_000_267_001,
         State_Primitive_Topology => 1_000_267_002,
         Viewport_With_Count => 1_000_267_003,
         Scissor_With_Count => 1_000_267_004,
         Vertex_Input_Binding_Stride => 1_000_267_005,
         Depth_Test_Enable => 1_000_267_006,
         Depth_Write_Enable => 1_000_267_007,
         Depth_Compare_Op => 1_000_267_008,
         Depths_Bounds_Test_Enable => 1_000_267_009,
         Stencil_Test_Enable => 1_000_267_010,
         State_Stencil_Op => 1_000_267_011,
         Raytracing_Pipeline_Stack_Size => 1_000_347_000,
         Vertex_Input => 1_000_352_000,
         Patch_Control_Points => 1_000_377_000,
         Rasterizer_Discard_Enable => 1_000_377_001,
         Depth_Bias_Enable => 1_000_377_002,
         State_Logic_Op => 1_000_377_003,
         Primitive_Restart_Enable => 1_000_377_004,
         Color_Write_Enable => 1_000_381_000,
         State_Tessellation_Domain_Origin => 1_000_455_002,
         Depth_Clamp_Enable => 1_000_455_003,
         State_Polygon_Mode => 1_000_455_004,
         Rasterization_Samples => 1_000_455_005,
         State_Sample_Mask => 1_000_455_006,
         Alpha_To_Converage_Enable => 1_000_455_007,
         Alpha_To_One_Enable => 1_000_455_008,
         Logic_Op_Enable => 1_000_455_009,
         Color_Blend_Enable => 1_000_455_010,
         Color_Blend_Equation => 1_000_455_011,
         Color_Write_Mask => 1_000_455_012,
         Rasterization_Stream => 1_000_455_013,
         State_Conservative_Rasterization_Mode => 1_000_455_014,
         Extra_Primitive_Overestimation_Size => 1_000_455_015,
         Depth_Clip_Enable => 1_000_455_016,
         Sample_Locations_Enable => 1_000_455_017,
         Color_Blend_Advanced => 1_000_455_018,
         Provoking_Vertex_Mode => 1_000_455_019,
         State_Line_Rasterization_Mode => 1_000_455_020,
         Line_Stipple_Enable => 1_000_455_021,
         Depth_Clip_Negative_One_To_One => 1_000_455_022,
         Viewport_W_Scaling_Enable => 1_000_455_023,
         State_Viewport_Swizzle => 1_000_455_024,
         Coverage_To_Color_Enable => 1_000_455_025,
         Coverage_To_Color_Location => 1_000_455_026,
         Coverage_Modulation_Mode => 1_000_455_027,
         Coverage_Modulation_Table_Enable => 1_000_455_028,
         Coverage_Modulation_Table => 1_000_455_029,
         Shading_Rate_Image_Enable => 1_000_455_030,
         Representative_Fragment_Test_Enable => 1_000_455_031,
         Coverage_Reduction_Mode => 1_000_455_032,
         Attachment_Feedback_Loop_Enable => 1_000_524_000);

    type Front_Face is (Counter_Clockwise,
                        Clockwise)
        with Convention => C;

    for Front_Face'Size use 32;

    for Front_Face use (Counter_Clockwise => 0,
                        Clockwise => 1);

    type Vertex_Input_Rate is (Rate_Vertex,
                               Rate_Instance)
        with Convention => C;

    for Vertex_Input_Rate'Size use 32;

    for Vertex_Input_Rate use (Rate_Vertex => 0,
                               Rate_Instance => 1);

    type Primitive_Topology is (Point_List,
                                Line_List,
                                Line_Strip,
                                Triangle_List,
                                Triangle_Strip,
                                Triangle_Fan,
                                Line_List_With_Adjacency,
                                Line_Strip_With_Adjacency,
                                Triangle_List_With_Adjacency,
                                Triangle_Strip_With_Adjacency,
                                Patch_List)
        with Convention => C;

    for Primitive_Topology'Size use 32;

    for Primitive_Topology use (Point_List => 0,
                                Line_List => 1,
                                Line_Strip => 2,
                                Triangle_List => 3,
                                Triangle_Strip => 4,
                                Triangle_Fan => 5,
                                Line_List_With_Adjacency => 6,
                                Line_Strip_With_Adjacency => 7,
                                Triangle_List_With_Adjacency => 8,
                                Triangle_Strip_With_Adjacency => 9,
                                Patch_List => 10);

    type Polygon_Mode is (Fill,
                          Line,
                          Point,
                          Fill_Rectangle)
        with Convention => C;

    for Polygon_Mode'Size use 32;

    for Polygon_Mode use (Fill => 0,
                          Line => 1,
                          Point => 2,
                          Fill_Rectangle => 1_000_153_000);
 
    type Stencil_Op is (Keep,
                        Zero,
                        Replace,
                        Increment_And_Clamp,
                        Decrement_And_Clamp,
                        Invert,
                        Increment_And_Wrap,
                        Decrement_And_Wrap)
        with Convention => C;

    for Stencil_Op'Size use 32;

    for Stencil_Op use (Keep => 0,
                        Zero => 1,
                        Replace => 2,
                        Increment_And_Clamp => 3,
                        Decrement_And_Clamp => 4,
                        Invert => 5,
                        Increment_And_Wrap => 6,
                        Decrement_And_Wrap => 7);

    type Logic_Op is (Clear,
                      Op_And,
                      Op_And_Reverse,
                      Copy,
                      Op_And_Inverted,
                      No_Op,
                      Op_Xor,
                      Op_Or,
                      Nor,
                      Equivalent,
                      Invert,
                      Op_Or_Reverse,
                      Copy_Inverted,
                      Op_Or_Inverted,
                      Nand,
                      Set)
        with Convention => C;

    for Logic_Op'Size use 32;

    for Logic_Op use (Clear => 0,
                      Op_And => 1,
                      Op_And_Reverse => 2,
                      Copy => 3,
                      Op_And_Inverted => 4,
                      No_Op => 5,
                      Op_Xor => 6,
                      Op_Or => 7,
                      Nor => 8,
                      Equivalent => 9,
                      Invert => 10,
                      Op_Or_Reverse => 11,
                      Copy_Inverted => 12,
                      Op_Or_Inverted => 13,
                      Nand => 14,
                      Set => 15);

    type Border_Color is (Float_Transparent_Black,
                          Int_Transparent_Black,
                          Float_Opaque_Black,
                          Int_Opaque_Black,
                          Float_Opaque_White,
                          Int_Opaque_White,
                          Float_Custom,
                          Int_Custom)
        with Convention => C;

    for Border_Color'Size use 32;

    for Border_Color use (Float_Transparent_Black => 0,
                          Int_Transparent_Black => 1,
                          Float_Opaque_Black => 2,
                          Int_Opaque_Black => 3,
                          Float_Opaque_White => 4,
                          Int_Opaque_White => 5,
                          Float_Custom => 1_000_287_003,
                          Int_Custom => 1_000_287_004);

    type Filter is (Nearest,
                    Linear,
                    Cubic_Img)
        with Convention => C;

    for Filter'Size use 32;

    for Filter use (Nearest => 0,
                    Linear => 1,
                    Cubic_Img => 1_000_015_000);

    type Sampler_Address_Mode is (Repeat,
                                  Mirrored_Repeat,
                                  Clamp_To_Edge,
                                  Clamp_To_Border,
                                  Mirror_Clamp_To_Edge)
        with Convention => C;

    for Sampler_Address_Mode'Size use 32;

    for Sampler_Address_Mode use (Repeat => 0,
                                  Mirrored_Repeat => 1,
                                  Clamp_To_Edge => 2,
                                  Clamp_To_Border => 3,
                                  Mirror_Clamp_To_Edge => 4);

    type Sampler_Mipmap_Mode is (Nearest,
                                 Linear)
        with Convention => C;

    for Sampler_Mipmap_Mode'Size use 32;

    for Sampler_Mipmap_Mode use (Nearest => 0,
                                 Linear => 1);
     
    type Descriptor_Type is (Type_Sampler,
                             Combined_Image_Sampler,
                             Sampled_Image,
                             Storage_Image,
                             Uniform_Texel_Buffer,
                             Storage_Texel_Buffer,
                             Uniform_Buffer,
                             Storage_Buffer,
                             Uniform_Buffer_Dynamic,
                             Storage_Buffer_Dynamic,
                             Input_Attachment,
                             Inline_Uniform_Block,
                             Acceleration_Structure_KHR,
                             Acceleration_Structure_NV,
                             Mutable,
                             Sample_Weight_Image,
                             Block_Match_Image)
        with Convention => C;

    for Descriptor_Type'Size use 32;

    for Descriptor_Type use (Type_Sampler => 0,
                             Combined_Image_Sampler => 1,
                             Sampled_Image => 2,
                             Storage_Image => 3,
                             Uniform_Texel_Buffer => 4,
                             Storage_Texel_Buffer => 5,
                             Uniform_Buffer => 6,
                             Storage_Buffer => 7,
                             Uniform_Buffer_Dynamic => 8,
                             Storage_Buffer_Dynamic => 9,
                             Input_Attachment => 10,
                             Inline_Uniform_Block => 1_000_138_000,
                             Acceleration_Structure_KHR => 1_000_150_000,
                             Acceleration_Structure_NV => 1_000_165_000,
                             Mutable => 1_000_351_000,
                             Sample_Weight_Image => 1_000_440_000,
                             Block_Match_Image => 1_000_440_001);
     
    type Attachment_Load_Op is (Load,
                                Clear,
                                Dont_Care,
                                None)
        with Convention => C;

    for Attachment_Load_Op'Size use 32;

    for Attachment_Load_Op use (Load => 0,
                                Clear => 1,
                                Dont_Care => 2,
                                None => 1_000_400_000);

    type Attachment_Store_Op is (Store,
                                 Dont_Care,
                                 None)
        with Convention => C;

    for Attachment_Store_Op'Size use 32;

    for Attachment_Store_Op use (Store => 0,
                                 Dont_Care => 1,
                                 None => 1_000_301_000);

    type Pipeline_Bind_Point is (Graphics,
                                 Compute,
                                 Ray_Tracing,
                                 Subpass_Shading)
        with Convention => C;

    for Pipeline_Bind_Point'Size use 32;

    for Pipeline_Bind_Point use (Graphics => 0,
                                 Compute => 1,
                                 Ray_Tracing => 1_000_165_000,
                                 Subpass_Shading => 1_000_369_003);

    type Command_Buffer_Level is (Primary,
                                  Secondary)
        with Convention => C;

    for Command_Buffer_Level'Size use 32;

    for Command_Buffer_Level use (Primary => 0,
                                  Secondary => 1);

    type Index_Type is (Uint16,
                        Uint32,
                        None,
                        Uint8)
        with Convention => C;

    for Index_Type'Size use 32;

    for Index_Type use (Uint16 => 0,
                        Uint32 => 1,
                        None => 1_000_165_000,
                        Uint8 => 1_000_265_000);

    type Subpass_Contents is (Inline,
                              Secondary_Command_Buffers)
        with Convention => C;

    for Subpass_Contents'Size use 32;

    for Subpass_Contents use (Inline => 0,
                              Secondary_Command_Buffers => 1);
 
    -- Vulkan 1.1
    type Point_Clipping_Behavior is (All_Clip_Planes,
                                     User_Clip_Planes_Only)
        with Convention => C;

    for Point_Clipping_Behavior'Size use 32;

    for Point_Clipping_Behavior use (All_Clip_Planes => 0,
                                     User_Clip_Planes_Only => 1);

    type Tessellation_Domain_Origin is (Upper_Left,
                                        Lower_Left)
        with Convention => C;

    for Tessellation_Domain_Origin'Size use 32;

    for Tessellation_Domain_Origin use (Upper_Left => 0,
                                        Lower_Left => 1);

    type Sampler_YCbCr_Model_Conversion is (RGB_Identity,
                                            YCbCr_Identity,
                                            YCbCr_709,
                                            YCbCr_601,
                                            YCbCr_2020)
        with Convention => C;

    for Sampler_YCbCr_Model_Conversion'Size use 32;

    for Sampler_YCbCr_Model_Conversion use (RGB_Identity => 0,
                                            YCbCr_Identity => 1,
                                            YCbCr_709 => 2,
                                            YCbCr_601 => 3,
                                            YCbCr_2020 => 4);

    type Sampler_YCbCr_Range is (ITU_Full,
                                 ITU_Narrow)
        with Convention => C;

    for Sampler_YCbCr_Range'Size use 32;

    for Sampler_YCbCr_Range use (ITU_Full => 0,
                                 ITU_Narrow => 1);

    type Chroma_Location is (Cosited_Even,
                             Midpoint)
        with Convention => C;

    for Chroma_Location'Size use 32;

    for Chroma_Location use (Cosited_Even => 0,
                             MidPoint => 1);
    
    type Descriptor_Update_Template_Type is (Type_Descriptor_Set,
                                             Push_Descriptions_KHR)
        with Convention => C;

    for Descriptor_Update_Template_Type'Size use 32;

    for Descriptor_Update_Template_Type use (Type_Descriptor_Set => 0,
                                             Push_Descriptions_KHR => 1);

    -- Vulkan 1.2
    type Driver_ID is (AMD_Proprietary,
                       AMD_Open_Source,
                       Mesa_RADV,
                       Nvidia_Proprietary,
                       Intel_Proprietary_Windows,
                       Intel_Open_Source_Mesa,
                       Imagination_Proprietary,
                       Qualcomm_Proprietary,
                       ARM_Proprietary,
                       Google_Swiftshader,
                       GGP_Proprietary,
                       Broadcom_Proprietary,
                       Mesa_LLVMPipe,
                       MoltenVK,
                       CoreAVI_Proprietary,
                       Juice_Proprietary,
                       Verisilicon_Proprietary,
                       Mesa_Turnip,
                       Mesa_V3DV,
                       Mesa_PanVK,
                       Samsung_Proprietary,
                       Mesa_Venus,
                       Mesa_Dozen,
                       Mesa_NVK,
                       Imagination_Open_Source_Mesa)
        with Convention => C;

    for Driver_ID'Size use 32;

    for Driver_ID use (AMD_Proprietary => 1,
                       AMD_Open_Source => 2,
                       Mesa_RADV => 3,
                       Nvidia_Proprietary => 4,
                       Intel_Proprietary_Windows => 5,
                       Intel_Open_Source_Mesa => 6,
                       Imagination_Proprietary => 7,
                       Qualcomm_Proprietary => 8,
                       ARM_Proprietary => 9,
                       Google_Swiftshader => 10,
                       GGP_Proprietary => 11,
                       Broadcom_Proprietary => 12,
                       Mesa_LLVMPipe => 13,
                       MoltenVK => 14,
                       CoreAVI_Proprietary => 15,
                       Juice_Proprietary => 16,
                       Verisilicon_Proprietary => 17,
                       Mesa_Turnip => 18,
                       Mesa_V3DV => 19,
                       Mesa_PanVK => 20,
                       Samsung_Proprietary => 21,
                       Mesa_Venus => 22,
                       Mesa_Dozen => 23,
                       Mesa_NVK => 24,
                       Imagination_Open_Source_Mesa => 25);
    
    type Shader_Float_Controls_Independence is (Independence_32_Bit_Only,
                                                Independence_All,
                                                None)
        with Convention => C;

    for Shader_Float_Controls_Independence'Size use 32;

    for Shader_Float_Controls_Independence use (Independence_32_Bit_Only => 0,
                                                Independence_All => 1,
                                                None => 2);

    type Sampler_Reduction_Mode is (Weighted_Average,
                                    Min,
                                    Max)
        with Convention => C;

    for Sampler_Reduction_Mode'Size use 32;

    for Sampler_Reduction_Mode use (Weighted_Average => 0,
                                    Min => 1,
                                    Max => 2);

    type Semaphore_Type is (Binary,
                            Timeline)
        with Convention => C;

    for Semaphore_Type'Size use 32;

    for Semaphore_Type use (Binary => 0,
                            Timeline => 1);

    -- Vulkan 1.4
    type Pipeline_Robustness_Buffer_Behavior is (Device_Default,
                                                 Disabled,
                                                 Robust_Buffer_Access,
                                                 Robust_Buffer_Access_2)
        with Convention => C;

    for Pipeline_Robustness_Buffer_Behavior'Size use 32;

    for Pipeline_Robustness_Buffer_Behavior use (Device_Default => 0,
                                                 Disabled => 1,
                                                 Robust_Buffer_Access => 2,
                                                 Robust_Buffer_Access_2 => 3);

    type Pipeline_Robustness_Image_Behavior is (Device_Default,
                                                Disabled,
                                                Robust_Image_Access,
                                                Robust_Image_Access_2)
        with Convention => C;

    for Pipeline_Robustness_Image_Behavior'Size use 32;

    for Pipeline_Robustness_Image_Behavior use (Device_Default => 0,
                                                Disabled => 1,
                                                Robust_Image_Access => 2,
                                                Robust_Image_Access_2 => 3);

    type Queue_Global_Priority is (Low,
                                   Medium,
                                   High,
                                   Realtime)
        with Convention => C;

    for Queue_Global_Priority'Size use 32;

    for Queue_Global_Priority use (Low => 128,
                                   Medium => 256,
                                   High => 512,
                                   Realtime => 1024);

    type Line_Rasterization_Mode is (Default,
                                     Rectangular,
                                     Bresenham,
                                     Rectangular_Smooth)
        with Convention => C;

    for Line_Rasterization_Mode'Size use 32;

    for Line_Rasterization_Mode use (Default => 0,
                                     Rectangular => 1,
                                     Bresenham => 2,
                                     Rectangular_Smooth => 3);

    -- Bitfields.
    type Access_Flags is new Flags;

    Access_No_Bit: constant Access_Flags := 0;
    Access_Indirect_Command_Read_Bit: constant Access_Flags := 16#00000001#;
    Access_Index_Read_Bit: constant Access_Flags := 16#00000002#;
    Access_Vertex_Attribute_Read_Bit: constant Access_Flags := 16#00000004#;
    Access_Uniform_Read_Bit: constant Access_Flags := 16#00000008#;
    Access_Input_Attachment_Read_Bit: constant Access_Flags := 16#00000010#;
    Access_Shader_Read_Bit: constant Access_Flags := 16#00000020#;
    Access_Shader_Write_Bit: constant Access_Flags := 16#00000040#;
    Access_Color_Attachment_Read_Bit: constant Access_Flags := 16#00000080#;
    Access_Color_Attachment_Write_Bit: constant Access_Flags := 16#00000100#;
    Access_Depth_Stencil_Attachment_Read_Bit:
        constant Access_Flags := 16#00000200#;
    Access_Depth_Stencil_Attachment_Write_Bit:
        constant Access_Flags := 16#00000400#;
    Access_Transfer_Read_Bit: constant Access_Flags := 16#00000800#;
    Access_Transfer_Write_Bit: constant Access_Flags := 16#00001000#;
    Access_Host_Read_Bit: constant Access_Flags := 16#00002000#;
    Access_Host_Write_Bit: constant Access_Flags := 16#00004000#;
    Access_Memory_Read_Bit: constant Access_Flags := 16#00008000#;
    Access_Memory_Write_Bit: constant Access_Flags := 16#00010000#;
    Access_Transform_Feedback_Write_Bit: constant Access_Flags := 16#02000000#;
    Access_Transform_Feedback_Counter_Read_Bit:
        constant Access_Flags := 16#04000000#;
    Access_Transform_Feedback_Counter_Write_Bit:
        constant Access_Flags := 16#08000000#;
    Access_Conditional_Rendering_Read_Bit:
        constant Access_Flags := 16#00100000#;
    Access_Color_Attachment_Read_Noncoherent_Bit:
        constant Access_Flags := 16#00080000#;
    Access_Acceleration_Structure_Read_Bit:
        constant Access_Flags := 16#00200000#;
    Access_Acceleration_Structure_Write_Bit:
        constant Access_Flags := 16#00400000#;
    Access_Fragment_Density_Map_Read_Bit:
        constant Access_Flags := 16#01000000#;
    Access_Fragment_Shading_Rate_Attachment_Read_Bit:
        constant Access_Flags := 16#00800000#;
    Access_Command_Preprocess_Read_Bit: constant Access_Flags := 16#00020000#;
    Access_Command_Preprocess_Write_Bit: constant Access_Flags := 16#00040000#;

    type Image_Aspect_Flags is new Flags;

    Image_Aspect_No_Bit: constant Image_Aspect_Flags := 0;
    Image_Aspect_Color_Bit: constant Image_Aspect_Flags := 16#00000001#;
    Image_Aspect_Depth_Bit: constant Image_Aspect_Flags := 16#00000002#;
    Image_Aspect_Stencil_Bit: constant Image_Aspect_Flags := 16#00000004#;
    Image_Aspect_Metadata_Bit: constant Image_Aspect_Flags := 16#00000008#;
    Image_Aspect_Plane_0_Bit: constant Image_Aspect_Flags := 16#00000010#;
    Image_Aspect_Plane_1_Bit: constant Image_Aspect_Flags := 16#00000020#;
    Image_Aspect_Plane_2_Bit: constant Image_Aspect_Flags := 16#00000040#;
    Image_Aspect_Memory_Plane_0_Bit:
        constant Image_Aspect_Flags := 16#00000080#;
    Image_Aspect_Memory_Plane_1_Bit:
        constant Image_Aspect_Flags := 16#00000100#;
    Image_Aspect_Memory_Plane_2_Bit:
        constant Image_Aspect_Flags := 16#00000200#;
    Image_Aspect_Memory_Plane_3_Bit:
        constant Image_Aspect_Flags := 16#00000400#;

    type Format_Feature_Flags is new Flags;

    Format_Feature_No_Bit: constant Format_Feature_Flags := 0;
    Format_Feature_Sampled_Image_Bit:
        constant Format_Feature_Flags := 16#00000001#;
    Format_Feature_Storage_Image_Bit:
        constant Format_Feature_Flags := 16#00000002#;
    Format_Feature_Storage_Image_Atomic_Bit:
        constant Format_Feature_Flags := 16#00000004#;
    Format_Feature_Uniform_Texel_Buffer_Bit:
        constant Format_Feature_Flags := 16#00000008#;
    Format_Feature_Storage_Texel_Buffer_Bit:
        constant Format_Feature_Flags := 16#00000010#;
    Format_Feature_Storage_Texel_Buffer_Atomic_Bit:
        constant Format_Feature_Flags := 16#00000020#;
    Format_Feature_Vertex_Buffer_Bit:
        constant Format_Feature_Flags := 16#00000040#;
    Format_Feature_Color_Attachment_Bit:
        constant Format_Feature_Flags := 16#00000080#;
    Format_Feature_Color_Attachment_Blend_Bit:
        constant Format_Feature_Flags := 16#00000100#;
    Format_Feature_Depth_Stencil_Attachment_Bit:
        constant Format_Feature_Flags := 16#00000200#;
    Format_Feature_Blit_Src_Bit: constant Format_Feature_Flags := 16#00000400#;
    Format_Feature_Blit_Dst_Bit: constant Format_Feature_Flags := 16#00000800#;
    Format_Feature_Sampled_Image_Filter_Linear_Bit:
        constant Format_Feature_Flags := 16#00001000#;
    Format_Feature_Transfer_Src_Bit:
        constant Format_Feature_Flags := 16#00004000#;
    Format_Feature_Transfer_Dst_Bit:
        constant Format_Feature_Flags := 16#00008000#;
    Format_Feature_Midpoint_Chroma_Samples_Bit:
        constant Format_Feature_Flags := 16#00020000#;
    Format_Feature_Sampled_Image_YCbCr_Conversion_Linear_Filter_Bit:
        constant Format_Feature_Flags := 16#00040000#;
    Format_Feature_Sampled_Image_YCbCr_Conversion_Separate_Reconstruction_Filter_Bit:
        constant Format_Feature_Flags := 16#00080000#;
    Format_Feature_Sampled_Image_YCbCr_Conversion_Chroma_Reconstruction_Explicit_Bit:
        constant Format_Feature_Flags := 16#00100000#;
    Format_Feature_Sampled_Image_YCbCr_Conversion_Chroma_Reconstruction_Explicit_Forceable_Bit:
        constant Format_Feature_Flags := 16#00200000#;
    Format_Feature_Disjoint_Bit: constant Format_Feature_Flags := 16#00400000#;
    Format_Feature_Cosited_Chroma_Samples_Bit:
        constant Format_Feature_Flags := 16#00800000#;
    Format_Feature_Sampled_Image_Filter_MinMax_Bit:
        constant Format_Feature_Flags := 16#00010000#;
    Format_Feature_Video_Decode_Output_Bit:
        constant Format_Feature_Flags := 16#02000000#;
    Format_Feature_Video_Decode_DPB_Bit:
        constant Format_Feature_Flags := 16#04000000#;
    Format_Feature_Acceleration_Structure_Vertex_Buffer_Bit:
        constant Format_Feature_Flags := 16#20000000#;
    Format_Feature_Sampled_Image_Filter_Cubic_Bit_Img:
        constant Format_Feature_Flags := 16#00002000#;
    Format_Feature_Fragment_Density_Map_Bit:
        constant Format_Feature_Flags := 16#01000000#;
    Format_Feature_Fragment_Shading_Rate_Attachment_Bit:
        constant Format_Feature_Flags := 16#40000000#;

    type Image_Create_Flags is new Flags;

    Image_Create_No_Bit: constant Image_Create_Flags := 0;
    Image_Create_Sparse_Binding_Bit:
        constant Image_Create_Flags := 16#00000001#;
    Image_Create_Sparse_Residency_Bit:
        constant Image_Create_Flags := 16#00000002#;
    Image_Create_Sparse_Aliased_Bit:
        constant Image_Create_Flags := 16#00000004#;
    Image_Create_Mutable_Format_Bit:
        constant Image_Create_Flags := 16#00000008#;
    Image_Create_Cube_Compatible_Bit:
        constant Image_Create_Flags := 16#00000010#;
    Image_Create_Alias_Bit: constant Image_Create_Flags := 16#00000400#;
    Image_Create_Split_Instance_Bind_Regions_Bit:
        constant Image_Create_Flags := 16#00000040#;
    Image_Create_2D_Array_Compatible_Bit:
        constant Image_Create_Flags := 16#00000020#;
    Image_Create_Block_Texel_View_Compatible_Bit:
        constant Image_Create_Flags := 16#00000080#;
    Image_Create_Extended_Usage_Bit:
        constant Image_Create_Flags := 16#00000100#;
    Image_Create_Protected_Bit: constant Image_Create_Flags := 16#00000800#;
    Image_Create_Disjoint_Bit: constant Image_Create_Flags := 16#00000200#;
    Image_Create_Corner_Sampled_Bit:
        constant Image_Create_Flags := 16#00002000#;
    Image_Create_Sample_Locations_Compatible_Depth_Bit:
        constant Image_Create_Flags := 16#00001000#;
    Image_Create_Subsampled_Bit: constant Image_Create_Flags := 16#00004000#;
    Image_Create_Descriptor_Buffer_Capture_Relay_Bit:
        constant Image_Create_Flags := 16#00010000#;
    Image_Create_Multisampled_Render_To_Single_Sampled_Bit:
        constant Image_Create_Flags := 16#00040000#;
    Image_Create_2D_View_Compatible_Bit:
        constant Image_Create_Flags := 16#00020000#;
    Fragment_Density_Map_Offset_Bit:
        constant Image_Create_Flags := 16#00008000#;

    type Sample_Count_Flags is new Flags;

    Sample_Count_No_Bit: constant Sample_Count_Flags := 0;
    Sample_Count_1_Bit: constant Sample_Count_Flags := 16#00000001#;
    Sample_Count_2_Bit: constant Sample_Count_Flags := 16#00000002#;
    Sample_Count_4_Bit: constant Sample_Count_Flags := 16#00000004#;
    Sample_Count_8_Bit: constant Sample_Count_Flags := 16#00000008#;
    Sample_Count_16_Bit: constant Sample_Count_Flags := 16#00000010#;
    Sample_Count_32_Bit: constant Sample_Count_Flags := 16#00000020#;
    Sample_Count_64_Bit: constant Sample_Count_Flags := 16#00000040#;

    type Image_Usage_Flags is new Flags;

    Image_Usage_No_Bit: constant Image_Usage_Flags := 0;
    Image_Usage_Transfer_Src_Bit: constant Image_Usage_Flags := 16#00000001#;
    Image_Usage_Transfer_Dst_Bit: constant Image_Usage_Flags := 16#00000002#;
    Image_Usage_Sampled_Bit: constant Image_Usage_Flags := 16#00000004#;
    Image_Usage_Storage_Bit: constant Image_Usage_Flags := 16#00000008#;
    Image_Usage_Color_Attachment_Bit:
        constant Image_Usage_Flags := 16#00000010#;
    Image_Usage_Depth_Stencil_Attachment_Bit:
        constant Image_Usage_Flags := 16#00000020#;
    Image_Usage_Transient_Attachment_Bit:
        constant Image_Usage_Flags := 16#00000040#;
    Image_Usage_Input_Attachment_Bit:
        constant Image_Usage_Flags := 16#00000080#;
    Image_Usage_Video_Decode_Dst_Bit:
        constant Image_Usage_Flags := 16#00000400#;
    Image_Usage_Video_Decode_Src_Bit:
        constant Image_Usage_Flags := 16#00000800#;
    Image_Usage_Video_Decode_DPB_Bit:
        constant Image_Usage_Flags := 16#00001000#;
    Image_Usage_Fragment_Density_Map_Bit:
        constant Image_Usage_Flags := 16#00000200#;
    Image_Usage_Fragment_Shading_Rate_Attachment_Bit:
        constant Image_Usage_Flags := 16#00000100#;
    Image_Usage_Attachment_Feedback_Loop_Bit:
        constant Image_Usage_Flags := 16#00080000#;
    Image_Usage_Invocation_Mask_Bit: constant Image_Usage_Flags := 16#00040000#;
    Image_Usage_Sample_Weight_Bit: constant Image_Usage_Flags := 16#00100000#;
    Image_Usage_Sample_Block_Match_Bit:
        constant Image_Usage_Flags := 16#00200000#;

    type Instance_Create_Flags is new Flags;

    Instance_Create_No_Bit: constant Instance_Create_Flags := 0;
    Instance_Create_Enumerate_Portability_Bit:
        constant Instance_Create_Flags := 16#00000001#;
  
    type Memory_Heap_Flags is new Flags;

    Memory_Heap_No_Bit: constant Memory_Heap_Flags := 0;
    Memory_Heap_Device_Local_Bit: constant Memory_Heap_Flags := 16#00000001#;
    Memory_Heap_Multi_Instance_Bit: constant Memory_Heap_Flags := 16#00000002#;

    type Memory_Property_Flags is new Flags;

    Memory_Property_No_Bit: constant Memory_Property_Flags := 0;
    Memory_Property_Device_Local_Bit:
        constant Memory_Property_Flags := 16#00000001#;
    Memory_Property_Host_Visible_Bit:
        constant Memory_Property_Flags := 16#00000002#;
    Memory_Property_Host_Coherent_Bit:
        constant Memory_Property_Flags := 16#00000004#;
    Memory_Property_Host_Cached_Bit:
        constant Memory_Property_Flags := 16#00000008#;
    Memory_Property_Lazily_Allocated_Bit:
        constant Memory_Property_Flags := 16#00000010#;
    Memory_Property_Protected_Bit:
        constant Memory_Property_Flags := 16#00000020#;
    Memory_Property_Device_Coherent_Bit:
        constant Memory_Property_Flags := 16#00000040#;
    Memory_Property_Device_Uncached_Bit:
        constant Memory_Property_Flags := 16#00000080#;
    Memory_Property_RDMA_Capable_Bit:
        constant Memory_Property_Flags := 16#00000100#;

    type Queue_Flags is new Flags;

    Queue_No_Bit: constant Queue_Flags := 0;
    Queue_Graphics_Bit: constant Queue_Flags := 16#00000001#;
    Queue_Compute_Bit: constant Queue_Flags := 16#00000002#;
    Queue_Transfer_Bit: constant Queue_Flags := 16#00000004#;
    Queue_Sparse_Binding_Bit: constant Queue_Flags := 16#00000008#;
    Queue_Protected_Bit: constant Queue_Flags := 16#00000010#;
    Queue_Video_Decode_Bit: constant Queue_Flags := 16#00000020#;
    Queue_Optical_Flow_Bit: constant Queue_Flags := 16#00000100#;
 
    type Device_Create_Flags is new Flags;

    Device_Create_No_Bit: constant Device_Create_Flags := 0;

    type Device_Queue_Create_Flags is new Flags;
    
    Device_Queue_Create_No_Bit: constant Device_Queue_Create_Flags := 0;
    Device_Queue_Create_Protected_Bit:
        constant Device_Queue_Create_Flags := 16#00000001#;

    type Pipeline_Stage_Flags is new Flags;

    Pipeline_Stage_No_Bit: constant Pipeline_Stage_Flags := 0;
    Pipeline_Stage_Top_Of_Pipe_Bit:
        constant Pipeline_Stage_Flags := 16#00000001#;
    Pipeline_Stage_Draw_Indirect_Bit:
        constant Pipeline_Stage_Flags := 16#00000002#;
    Pipeline_Stage_Vertex_Input_Bit:
        constant Pipeline_Stage_Flags := 16#00000004#;
    Pipeline_Stage_Vertex_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000008#;
    Pipeline_Stage_Tessellation_Control_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000010#;
    Pipeline_Stage_Tessellation_Evaluation_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000020#;
    Pipeline_Stage_Geometry_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000040#;
    Pipeline_Stage_Fragment_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000080#;
    Pipeline_Stage_Early_Fragment_Tests_Bit:
        constant Pipeline_Stage_Flags := 16#00000100#;
    Pipeline_Stage_Late_Fragment_Tests_Bit:
        constant Pipeline_Stage_Flags := 16#00000200#;
    Pipeline_Stage_Color_Attachment_Output_Bit:
        constant Pipeline_Stage_Flags := 16#00000400#;
    Pipeline_Stage_Compute_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00000800#;
    Pipeline_Stage_Transfer_Bit: constant Pipeline_Stage_Flags := 16#00001000#;
    Pipeline_Stage_Bottom_Of_Pipe_Bit:
        constant Pipeline_Stage_Flags := 16#00002000#;
    Pipeline_Stage_Host_Bit: constant Pipeline_Stage_Flags := 16#00004000#;
    Pipeline_Stage_All_Graphics_Bit:
        constant Pipeline_Stage_Flags := 16#00008000#;
    Pipeline_Stage_All_Commands_Bit:
        constant Pipeline_Stage_Flags := 16#00010000#;
    Pipeline_Stage_Transform_Feedback_Bit:
        constant Pipeline_Stage_Flags := 16#01000000#;
    Pipeline_Stage_Conditional_Rendering_Bit:
        constant Pipeline_Stage_Flags := 16#00040000#;
    Pipeline_Stage_Acceleration_Structure_Build_Bit:
        constant Pipeline_Stage_Flags := 16#02000000#;
    Pipeline_Stage_Ray_Tracing_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00200000#;
    Pipeline_Stage_Fragment_Density_Process_Bit:
        constant Pipeline_Stage_Flags := 16#00800000#;
    Pipeline_Stage_Fragment_Shading_Rate_Attachment_Bit:
        constant Pipeline_Stage_Flags := 16#00400000#;
    Pipeline_Stage_Command_Preprocess_Bit:
        constant Pipeline_Stage_Flags := 16#00020000#;
    Pipeline_Stage_Task_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00080000#;
    Pipeline_Stage_Mesh_Shader_Bit:
        constant Pipeline_Stage_Flags := 16#00100000#;

    type Memory_Map_Flags is new Flags;

    Memory_Map_No_Bit: constant Memory_Map_Flags := 0;

    type Sparse_Memory_Bind_Flags is new Flags;

    Sparse_Memory_Bind_No_Bit: constant Sparse_Memory_Bind_Flags := 0;
    Sparse_Memory_Bind_Metadata_Bit:
        constant Sparse_Memory_Bind_Flags := 16#00000001#;

    type Sparse_Image_Format_Flags is new Flags;

    Sparse_Image_Format_No_Bit: constant Sparse_Image_Format_Flags := 0;
    Sparse_Image_Format_Single_Miptail_Bit:
        constant Sparse_Image_Format_Flags := 16#00000001#;
    Sparse_Image_Format_Aligned_Mip_Size_Bit:
        constant Sparse_Image_Format_Flags := 16#00000002#;
    Sparse_Image_Format_Nonstandard_Block_Size_Bit:
        constant Sparse_Image_Format_Flags := 16#00000004#;
    
    type Fence_Create_Flags is new Flags;

    Fence_Create_No_Bit: constant Fence_Create_Flags := 0;
    Fence_Create_Signaled_Bit: constant Fence_Create_Flags := 16#00000001#;

    type Semaphore_Create_Flags is new Flags;

    Semaphore_Create_No_Bit: constant Semaphore_Create_Flags := 0;

    type Event_Create_Flags is new Flags;

    Event_Create_No_Bit: constant Event_Create_Flags := 0;
    Event_Create_Device_Only_Bit: constant Event_Create_Flags := 16#00000001#;

    type Query_Pipeline_Statistic_Flags is new Flags;

    Query_Pipeline_Statistic_No_Bit:
        constant Query_Pipeline_Statistic_Flags := 0;
    Query_Pipeline_Statistic_Input_Assembly_Vertices_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000001#;
    Query_Pipeline_Statistic_Input_Assembly_Primitives_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000002#;
    Query_Pipeline_Statistic_Vertex_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000004#;
    Query_Pipeline_Statistic_Geometry_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000008#;
    Query_Pipeline_Statistic_Geometery_Shader_Primitives_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000010#;
    Query_Pipeline_Statistic_Clipping_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000020#;
    Query_Pipeline_Statistic_Clipping_Primitives_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000040#;
    Query_Pipeline_Statistic_Fragment_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000080#;
    Query_Pipeline_Statistic_Tessellation_Control_Shader_Patches_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000100#;
    Query_Pipeline_Statistic_Tessellation_Evaluation_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000200#;
    Query_Pipeline_Statistic_Compute_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000400#;
    Query_Pipeline_Statistic_Task_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00000800#;
    Query_Pipeline_Statistic_Mesh_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00001000#;
    Query_Pipeline_Statistic_Cluster_Culling_Shader_Invocations_Bit:
        constant Query_Pipeline_Statistic_Flags := 16#00002000#;

    type Query_Pool_Create_Flags is new Flags;

    Query_Pool_Create_No_Bit: constant Query_Pool_Create_Flags := 0;

    type Query_Result_Flags is new Flags;

    Query_Result_No_Bit: constant Query_Result_Flags := 0;
    Query_Result_64_Bit: constant Query_Result_Flags := 16#00000001#;
    Query_Result_Wait_Bit: constant Query_Result_Flags := 16#00000002#;
    Query_Result_With_Availability_Bit:
        constant Query_Result_Flags := 16#00000004#;
    Query_Result_Partial_Bit: constant Query_Result_Flags := 16#00000008#;
    Query_Result_With_Status_Bit: constant Query_Result_Flags := 16#00000010#;

    type Buffer_Create_Flags is new Flags;

    Buffer_Create_No_Bit: constant Buffer_Create_Flags := 0;
    Buffer_Create_Sparse_Binding_Bit:
        constant Buffer_Create_Flags := 16#00000001#;
    Buffer_Create_Sparse_Residency_Bit:
        constant Buffer_Create_Flags := 16#00000002#;
    Buffer_Create_Sparse_Aliased_Bit:
        constant Buffer_Create_Flags := 16#00000004#;
    Buffer_Create_Protected_Bit: constant Buffer_Create_Flags := 16#00000008#;
    Buffer_Create_Device_Address_Capture_Replay_Bit:
        constant Buffer_Create_Flags := 16#00000010#;
    Buffer_Create_Descriptor_Buffer_Capture_Replay_Bit:
        constant Buffer_Create_Flags := 16#00000020#;

    type Buffer_Usage_Flags is new Flags;

    Buffer_Usage_No_Bit: constant Buffer_Usage_Flags := 0;
    Buffer_Usage_Transfer_Src_Bit: constant Buffer_Usage_Flags := 16#00000001#;
    Buffer_Usage_Transfer_Dst_Bit: constant Buffer_Usage_Flags := 16#00000002#;
    Buffer_Usage_Uniform_Texel_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000004#;
    Buffer_Usage_Storage_Texel_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000008#;
    Buffer_Usage_Uniform_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000010#;
    Buffer_Usage_Storage_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000020#;
    Buffer_Usage_Index_Buffer_Bit: constant Buffer_Usage_Flags := 16#00000040#;
    Buffer_Usage_Vertex_Buffer_Bit: constant Buffer_Usage_Flags := 16#00000080#;
    Buffer_Usage_Indirect_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000100#;
    Buffer_Usage_Shader_Device_Address_Bit:
        constant Buffer_Usage_Flags := 16#00020000#;
    Buffer_Usage_Video_Decode_Src_Bit:
        constant Buffer_Usage_Flags := 16#00002000#;
    Buffer_Usage_Video_Decode_Dst_Bit:
        constant Buffer_Usage_Flags := 16#00004000#;
    Buffer_Usage_Transform_Feedback_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00000800#;
    Buffer_Usage_Transform_Feedback_Counter_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00001000#;
    Buffer_Usage_Conditional_Rendering_Bit:
        constant Buffer_Usage_Flags := 16#00000200#;
    Buffer_Usage_Acceleration_Structure_Build_Input_Read_Only_Bit:
        constant Buffer_Usage_Flags := 16#00080000#;
    Buffer_Usage_Acceleration_Structure_Storage_Bit:
        constant Buffer_Usage_Flags := 16#00100000#;
    Buffer_Usage_Shader_Binding_Table_Bit:
        constant Buffer_Usage_Flags := 16#00000400#;
    Buffer_Usage_Sampler_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00200000#;
    Buffer_Usage_Resource_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#00400000#;
    Buffer_Usage_Push_Descriptors_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags := 16#04000000#;
    Buffer_Usage_Micromap_Build_Input_Read_Only_Bit:
        constant Buffer_Usage_Flags := 16#00800000#;
    Buffer_Usage_Micromap_Storage_Bit:
        constant Buffer_Usage_Flags := 16#01000000#;

    type Buffer_View_Create_Flags is new Flags;

    Buffer_View_Create_No_Bit: constant Buffer_View_Create_Flags := 0;

    type Image_View_Create_Flags is new Flags;

    Image_View_Create_No_Bit: constant Image_View_Create_Flags := 0;
    Image_View_Create_Fragment_Density_Map_Dynamic_Bit:
        constant Image_View_Create_Flags := 16#00000001#;
    Image_View_Create_Descriptor_Buffer_Capture_Replay_Bit:
        constant Image_View_Create_Flags := 16#00000004#;
    Image_View_Create_Fragment_Density_Map_Deferred_Bit:
        constant Image_View_Create_Flags := 16#00000002#;

    type Shader_Module_Create_Flags is new Flags;

    Shader_Module_Create_No_Bit: constant Shader_Module_Create_Flags := 0;

    type Pipeline_Cache_Create_Flags is new Flags;

    Pipeline_Cache_Create_No_Bit: constant Pipeline_Cache_Create_Flags := 0;
    Pipeline_Cache_Create_Externally_Synchronized_Bit:
        constant Pipeline_Cache_Create_Flags := 16#00000001#;

    type Color_Component_Flags is new Flags;
    
    Color_Component_No_Bit: constant Color_Component_Flags := 0;
    Color_Component_R_Bit: constant Color_Component_Flags := 16#00000001#;
    Color_Component_G_Bit: constant Color_Component_Flags := 16#00000002#;
    Color_Component_B_Bit: constant Color_Component_Flags := 16#00000004#;
    Color_Component_A_Bit: constant Color_Component_Flags := 16#00000008#;

    type Pipeline_Create_Flags is new Flags;

    Pipeline_Create_No_Bit: constant Pipeline_Create_Flags := 0;
    Pipeline_Create_Disable_Optimization_Bit:
        constant Pipeline_Create_Flags := 16#00000001#;
    Pipeline_Create_Allow_Derivatives_Bit:
        constant Pipeline_Create_Flags := 16#00000002#;
    Pipeline_Create_Derivative_Bit:
        constant Pipeline_Create_Flags := 16#00000004#;
    Pipeline_Create_View_Index_From_Device_Index_Bit:
        constant Pipeline_Create_Flags := 16#00000008#;
    Pipeline_Create_Dispatch_Base_Bit:
        constant Pipeline_Create_Flags := 16#00000010#;
    Pipeline_Create_Fail_On_Pipeline_Compile_Required_Bit:
        constant Pipeline_Create_Flags := 16#00000100#;
    Pipeline_Create_Early_Return_On_Failure_Bit:
        constant Pipeline_Create_Flags := 16#00000200#;
    Pipeline_Create_Rendering_Fragment_Shading_Rate_Attachment_Bit:
        constant Pipeline_Create_Flags := 16#00200000#;
    Pipeline_Create_Rendering_Fragment_Density_Map_attachment_Bit:
        constant Pipeline_Create_Flags := 16#00400000#;
    Pipeline_Create_Ray_Tracing_No_Null_Any_Hit_Shaders_Bit:
        constant Pipeline_Create_Flags := 16#00004000#;
    Pipeline_Create_Ray_Tracing_No_Null_Closest_Hit_Shaders_Bit:
        constant Pipeline_Create_Flags := 16#00008000#;
    Pipeline_Create_Ray_Tracing_No_Null_Miss_Shaders_Bit:
        constant Pipeline_Create_Flags := 16#00010000#;
    Pipeline_Create_Ray_Tracing_No_Null_Intersection_Shaders_Bit:
        constant Pipeline_Create_Flags := 16#00020000#;
    Pipeline_Create_Ray_Tracing_Skip_Triangles_Bit:
        constant Pipeline_Create_Flags := 16#00001000#;
    Pipeline_Create_Ray_Tracing_Skip_AABBS_Bit:
        constant Pipeline_Create_Flags := 16#00002000#;
    Pipeline_Create_Ray_Tracing_Shader_Group_Handle_Capture_Replay_Bit:
        constant Pipeline_Create_Flags := 16#00080000#;
    Pipeline_Create_Defer_Compile_Bit:
        constant Pipeline_Create_Flags := 16#00000020#;
    Pipeline_Create_Capture_Statistics_Bit:
        constant Pipeline_Create_Flags := 16#00000040#;
    Pipeline_Create_Capture_Internal_Representations_Bit:
        constant Pipeline_Create_Flags := 16#00000080#;
    Pipeline_Create_Indirect_Bindable_Bit:
        constant Pipeline_Create_Flags := 16#00040000#;
    Pipeline_Create_Library_Bit: constant Pipeline_Create_Flags := 16#00000800#;
    Pipeline_Create_Descriptor_Buffer_Bit:
        constant Pipeline_Create_Flags := 16#20000000#;
    Pipeline_Create_Retain_Link_Time_Optimization_Info_Bit:
        constant Pipeline_Create_Flags := 16#00800000#;
    Pipeline_Create_Link_Time_Optimization_Bit:
        constant Pipeline_Create_Flags := 16#00000400#;
    Pipeline_Create_Ray_Tracing_Allow_Motion_Bit:
        constant Pipeline_Create_Flags := 16#00100000#;
    Pipeline_Create_Color_Attachment_Feedback_Loop_Bit:
        constant Pipeline_Create_Flags := 16#02000000#;
    Pipeline_Create_Depth_Stencil_Attachment_Feedback_Loop_Bit:
        constant Pipeline_Create_Flags := 16#04000000#;
    Pipeline_Create_Ray_Tracing_Opacity_Micromap_Bit:
        constant Pipeline_Create_Flags := 16#01000000#;
    Pipeline_Create_No_Protected_Access_Bit:
        constant Pipeline_Create_Flags := 16#08000000#;
    Pipeline_Create_Protected_Access_Only_Bit:
        constant Pipeline_Create_Flags := 16#40000000#;

    type Pipeline_Shader_Stage_Create_Flags is new Flags;

    Pipeline_Shader_Stage_Create_No_Bit:
        constant Pipeline_Shader_Stage_Create_Flags := 0;
    Pipeline_Shader_Stage_Create_Allow_Varying_Subgroup_Size_Bit:
        constant Pipeline_Shader_Stage_Create_Flags := 16#00000001#;
    Pipeline_Shader_Stage_Create_Require_Full_Subgroups_Bit:
        constant Pipeline_Shader_Stage_Create_Flags := 16#00000002#;

    type Shader_Stage_Flags is new Flags;

    Shader_Stage_No_Bit: constant Shader_Stage_Flags := 0;
    Shader_Stage_Vertex_Bit: constant Shader_Stage_Flags := 16#00000001#;
    Shader_Stage_Tessellation_Control_Bit:
        constant Shader_Stage_Flags := 16#00000002#;
    Shader_Stage_Tessellation_Evaluation_Bit:
        constant Shader_Stage_Flags := 16#00000004#;
    Shader_Stage_Geometry_Bit: constant Shader_Stage_Flags := 16#00000008#;
    Shader_Stage_Fragment_Bit: constant Shader_Stage_Flags := 16#00000010#;
    Shader_Stage_Compute_Bit: constant Shader_Stage_Flags := 16#00000020#;
    Shader_Stage_All_Graphics_Bit: constant Shader_Stage_Flags := 16#0000001f#;
    Shader_Stage_All_Bit: constant Shader_Stage_Flags := 16#7fffffff#;
    Shader_Stage_Raygen_Bit: constant Shader_Stage_Flags := 16#00000100#;
    Shader_Stage_Any_Hit_Bit: constant Shader_Stage_Flags := 16#00000200#;
    Shader_Stage_Closest_Hit_Bit: constant Shader_Stage_Flags := 16#00000400#;
    Shader_Stage_Miss_Bit: constant Shader_Stage_Flags := 16#00000800#;
    Shader_Stage_Intersection_Bit: constant Shader_Stage_Flags := 16#00001000#;
    Shader_Stage_Callable_Bit: constant Shader_Stage_Flags := 16#00002000#;
    Shader_Stage_Task_Bit: constant Shader_Stage_Flags := 16#00000040#;
    Shader_Stage_Mesh_Bit: constant Shader_Stage_Flags := 16#00000080#;
    Shader_Stage_Subpass_Shading_Bit:
        constant Shader_Stage_Flags := 16#00004000#;
    Shader_Stage_Cluster_Culling_Bit:
        constant Shader_Stage_Flags := 16#00080000#;

    type Cull_Mode_Flags is new Flags;

    Cull_Mode_None: constant Cull_Mode_Flags := 0;
    Cull_Mode_Front_Bit: constant Cull_Mode_Flags := 16#00000001#;
    Cull_Mode_Back_Bit: constant Cull_Mode_Flags := 16#00000002#;
    Cull_Mode_Front_And_Back: constant Cull_Mode_Flags := 16#00000003#;

    type Pipeline_Vertex_Input_State_Create_Flags is new Flags;

    Pipeline_Vertex_Input_State_Create_No_Bit:
        constant Pipeline_Vertex_Input_State_Create_Flags := 0;

    type Pipeline_Input_Assembly_State_Create_Flags is new Flags;

    Pipeline_Input_Assembly_State_Create_No_Bit:
        constant Pipeline_Input_Assembly_State_Create_Flags := 0;

    type Pipeline_Tessellation_State_Create_Flags is new Flags;

    Pipeline_Tessellation_State_Create_No_Bit:
        constant Pipeline_Tessellation_State_Create_Flags := 0;

    type Pipeline_Viewport_State_Create_Flags is new Flags;

    Pipeline_Viewport_State_Create_No_Bit:
        constant Pipeline_Viewport_State_Create_Flags := 0;

    type Pipeline_Rasterization_State_Create_Flags is new Flags;

    Pipeline_Rasterization_State_Create_No_Bit:
        constant Pipeline_Rasterization_State_Create_Flags := 0;

    type Pipeline_Multisample_State_Create_Flags is new Flags;

    Pipeline_Multisample_State_Create_No_Bit:
        constant Pipeline_Multisample_State_Create_Flags := 0;

    type Pipeline_Depth_Stencil_State_Create_Flags is new Flags;

    Pipeline_Depth_Stencil_State_Create_No_Bit:
        constant Pipeline_Depth_Stencil_State_Create_Flags := 0;
    Pipeline_Depth_Stencil_State_Create_Rasterization_Order_Attachment_Depth_Access_Bit:
        constant Pipeline_Depth_Stencil_State_Create_Flags := 16#00000001#;
    Pipeline_Depth_Stencil_State_Create_Rasterization_Order_Attachment_Stencil_Access_Bit:
        constant Pipeline_Depth_Stencil_State_Create_Flags := 16#00000002#;

    type Pipeline_Color_Blend_State_Create_Flags is new Flags;
 
    Pipeline_Color_Blend_State_Create_No_Bit:
        constant Pipeline_Color_Blend_State_Create_Flags := 0;
    Pipeline_Color_Blend_State_Create_Rasterization_Order_Attachment_Access_Bit:
        constant Pipeline_Color_Blend_State_Create_Flags := 16#00000001#;

    type Pipeline_Dynamic_State_Create_Flags is new Flags;

    Pipeline_Dynamic_State_Create_No_Bit:
        constant Pipeline_Dynamic_State_Create_Flags := 0;

    type Pipeline_Layout_Create_Flags is new Flags;

    Pipeline_Layout_Create_No_Bit: constant Pipeline_Layout_Create_Flags := 0;
    Pipeline_Layout_Create_Independent_Sets_Bit:
        constant Pipeline_Layout_Create_Flags := 16#00000002#;

    type Sampler_Create_Flags is new Flags;

    Sampler_Create_No_Bit: constant Sampler_Create_Flags := 0;
    Sampler_Create_Subsampled_Bit:
        constant Sampler_Create_Flags := 16#00000001#;
    Sampler_Create_Subsampled_Coarse_Reconstruction_Bit:
        constant Sampler_Create_Flags := 16#00000002#;
    Sampler_Create_Descriptor_Buffer_Capture_Replay_Bit:
        constant Sampler_Create_Flags := 16#00000008#;
    Sampler_Create_Non_Seamless_Cube_Map_Bit:
        constant Sampler_Create_Flags := 16#00000004#;
    Sampler_Create_Image_Processing_Bit:
        constant Sampler_Create_Flags := 16#00000010#;

    type Descriptor_Pool_Create_Flags is new Flags;

    Descriptor_Pool_Create_No_Bit: constant Descriptor_Pool_Create_Flags := 0;
    Descriptor_Pool_Create_Free_Descriptor_Set_Bit:
        constant Descriptor_Pool_Create_Flags := 16#00000001#;
    Descriptor_Pool_Create_Update_After_Bind_Bit:
        constant Descriptor_Pool_Create_Flags := 16#00000002#;
    Descriptor_Pool_Create_Host_Only_Bit:
        constant Descriptor_Pool_Create_Flags := 16#00000004#;

    type Descriptor_Pool_Reset_Flags is new Flags;

    Descriptor_Pool_Reset_No_Bit: constant Descriptor_Pool_Reset_Flags := 0;

    type Descriptor_Set_Layout_Create_Flags is new Flags;

    Descriptor_Set_Layout_Create_No_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 0;
    Descriptor_Set_Layout_Create_Update_After_Bind_Pool_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 16#00000002#;
    Descriptor_Set_Layout_Create_Push_Descriptor_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 16#00000001#;
    Descriptor_Set_Layout_Create_Descriptor_Buffer_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 16#00000010#;
    Descriptor_Set_Layout_Create_Embedded_Immutable_Samplers_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 16#00000020#;
    Descriptor_Set_Layout_Create_Host_Only_Pool_Bit:
        constant Descriptor_Set_Layout_Create_Flags := 16#00000004#;
    
    type Attachment_Description_Flags is new Flags;

    Attachment_Description_No_Bit: constant Attachment_Description_Flags := 0;
    Attachment_Description_May_Alias_Bit:
        constant Attachment_Description_Flags := 16#00000001#;

    type Dependency_Flags is new Flags;

    Dependency_No_Bit: constant Dependency_Flags := 0;
    Dependency_By_Region_Bit: constant Dependency_Flags := 16#00000001#;
    Dependency_Device_Group_Bit: constant Dependency_Flags := 16#00000004#;
    Dependency_View_Local_Bit: constant Dependency_Flags := 16#00000002#;
    Dependency_Feedback_Loop_Bit: constant Dependency_Flags := 16#00000008#;

    type Framebuffer_Create_Flags is new Flags;

    Framebuffer_Create_No_Bit: constant Framebuffer_Create_Flags := 0;
    Framebuffer_Create_Imageless_Bit:
        constant Framebuffer_Create_Flags := 16#00000001#;

    type Render_Pass_Create_Flags is new Flags;

    Render_Pass_Create_No_Bit: constant Render_Pass_Create_Flags := 0;
    Render_Pass_Create_Transform_Bit:
        constant Render_Pass_Create_Flags := 16#00000002#;

    type Subpass_Description_Flags is new Flags;

    Subpass_Description_No_Bit: constant Subpass_Description_Flags := 0;
    Subpass_Description_Per_View_Attributes_Bit:
        constant Subpass_Description_Flags := 16#00000001#;
    Subpass_Description_Per_View_Position_X_Only_Bit:
        constant Subpass_Description_Flags := 16#00000002#;
    Subpass_Description_Fragment_Region_Bit:
        constant Subpass_Description_Flags := 16#00000004#;
    Subpass_Description_Shader_Resolve_Bit:
        constant Subpass_Description_Flags := 16#00000008#;
    Subpass_Description_Rasterization_Order_Attachment_Color_Access_Bit:
        constant Subpass_Description_Flags := 16#00000010#;
    Subpass_Description_Rasterization_Order_Attachment_Depth_Access_Bit:
        constant Subpass_Description_Flags := 16#00000020#;
    Subpass_Description_Rasterization_Order_Attachment_Stencil_Access_Bit:
        constant Subpass_Description_Flags := 16#00000040#;
    Subpass_Description_Enable_Legacy_Dithering_Bit:
        constant Subpass_Description_Flags := 16#00000080#;
     
    type Command_Pool_Create_Flags is new Flags;

    Command_Pool_Create_No_Bit: constant Command_Pool_Create_Flags := 0;
    Command_Pool_Create_Transient_Bit:
        constant Command_Pool_Create_Flags := 16#00000001#;
    Command_Pool_Create_Reset_Command_Buffer_Bit:
        constant Command_Pool_Create_Flags := 16#00000002#;
    Command_Pool_Create_Protected_Bit:
        constant Command_Pool_Create_Flags := 16#00000004#;

    type Command_Pool_Reset_Flags is new Flags;

    Command_Pool_Reset_No_Bit: constant Command_Pool_Reset_Flags := 0;
    Command_Pool_Reset_Release_Resources_Bit:
        constant Command_Pool_Reset_Flags := 16#00000001#;

    type Command_Buffer_Usage_Flags is new Flags;

    Command_Buffer_Usage_No_Bit: constant Command_Buffer_Usage_Flags := 0;
    Command_Buffer_Usage_One_Time_Submit_Bit:
        constant Command_Buffer_Usage_Flags := 16#00000001#;
    Command_Buffer_Usage_Render_Pass_Continue_Bit:
        constant Command_Buffer_Usage_Flags := 16#00000002#;
    Command_Buffer_Usage_Simultaneous_Use_Bit: 
        constant Command_Buffer_Usage_Flags := 16#00000004#;

    type Query_Control_Flags is new Flags;

    Query_Control_No_Bit: constant Query_Control_Flags := 0;
    Query_Control_Precise_Bit: constant Query_Control_Flags := 16#00000001#;

    type Command_Buffer_Reset_Flags is new Flags;

    Command_Buffer_Reset_No_Bit: constant Command_Buffer_Reset_Flags := 0;
    Command_Buffer_Reset_Release_Resources_Bit:
        constant Command_Buffer_Reset_Flags := 16#00000001#;

    type Stencil_Face_Flags is new Flags;

    Stencil_Face_No_Bit: constant Stencil_Face_Flags := 0;
    Stencil_Face_Front_Bit: constant Stencil_Face_Flags := 16#00000001#;
    Stencil_Face_Back_Bit: constant Stencil_Face_Flags := 16#00000002#;
    Stencil_Face_Front_And_Back: constant Stencil_Face_Flags := 16#00000003#;

    -- Vulkan 1.1
    type Subgroup_Feature_Flags is new Flags;

    Subgroup_Feature_No_Bit: constant Subgroup_Feature_Flags := 0;
    Subgroup_Feature_Basic_Bit:
        constant Subgroup_Feature_Flags := 16#00000001#;
    Subgroup_Feature_Vote_Bit: constant Subgroup_Feature_Flags := 16#00000002#;
    Subgroup_Feature_Arithmetic_Bit:
        constant Subgroup_Feature_Flags := 16#00000004#;
    Subgroup_Feature_Ballot_Bit:
        constant Subgroup_Feature_Flags := 16#00000008#;
    Subgroup_Feature_Shuffle_Bit:
        constant Subgroup_Feature_Flags := 16#00000010#;
    Subgroup_Feature_Shuffle_Relative_Bit:
        constant Subgroup_Feature_Flags := 16#00000020#;
    Subgroup_Feature_Clustered_Bit:
        constant Subgroup_Feature_Flags := 16#00000040#;
    Subgroup_Feature_Quad_Bit: constant Subgroup_Feature_Flags := 16#00000080#;
    Subgroup_Feature_Partitioned_Bit:
        constant Subgroup_Feature_Flags := 16#00000100#;

    type Peer_Memory_Feature_Flags is new Flags;

    Peer_Memory_Feature_No_Bit: constant Peer_Memory_Feature_Flags := 0;
    Peer_Memory_Feature_Copy_Src_Bit:
        constant Peer_Memory_Feature_Flags := 16#00000001#;
    Peer_Memory_Feature_Copy_Dst_Bit:
        constant Peer_Memory_Feature_Flags := 16#00000002#;
    Peer_Memory_Feature_Generic_Src_Bit:
        constant Peer_Memory_Feature_Flags := 16#00000004#;
    Peer_Memory_Feature_Generic_Dst_Bit:
        constant Peer_Memory_Feature_Flags := 16#00000008#;

    type Memory_Allocate_Flags is new Flags;

    Memory_Allocate_No_Bit: constant Memory_Allocate_Flags := 0;
    Memory_Allocate_Device_Mask_Bit:
        constant Memory_Allocate_Flags := 16#00000001#;
    Memory_Allocate_Device_Address_Bit:
        constant Memory_allocate_Flags := 16#00000002#;
    Memory_Allocate_Device_Address_Capture_Replay_Bit:
        constant Memory_Allocate_Flags := 16#00000004#;

    type Command_Pool_Trim_Flags is new Flags;

    Command_Pool_Trim_No_Bit: constant Command_Pool_Trim_Flags := 0;

    type Descriptor_Update_Template_Create_Flags is new Flags;

    Descriptor_Update_Template_Create_No_Bit:
        constant Descriptor_Update_Template_Create_Flags := 0;

    type External_Memory_Handle_Type_Flags is new Flags;

    External_Memory_Handle_Type_No_Bit:
        constant External_Memory_Handle_Type_Flags := 0;
    External_Memory_Handle_Type_Opaque_FD_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000001#;
    External_Memory_Handle_Type_Opaque_Win32_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000002#;
    External_Memory_Handle_Type_Opaque_Win32_KMT_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000004#;
    External_Memory_Handle_Type_D3D11_Texture_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000008#;
    External_Memory_Handle_Type_D3D11_Texture_KMT_BIT:
        constant External_Memory_Handle_Type_Flags := 16#00000010#;
    External_Memory_Handle_Type_D3D12_Heap_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000020#;
    External_Memory_Handle_Type_D3D12_Resource_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000040#;
    External_Memory_Handle_Type_DMA_Buf_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000200#;
    External_Memory_Handle_Type_Android_Hardware_Buffer_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000400#;
    External_Memory_Handle_Type_Host_Allocation_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000080#;
    External_Memory_Handle_Type_Host_Mapped_Foreign_Memory_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000100#;
    External_Memory_Handle_Type_Zircon_VMO_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00000800#;
    External_Memory_Handle_Type_RDMA_Address_Bit:
        constant External_Memory_Handle_Type_Flags := 16#00001000#;

    type External_Memory_Feature_Flags is new Flags;

    External_Memory_Feature_No_Bit: constant External_Memory_Feature_Flags := 0;
    External_Memory_Feature_Dedicated_Only_Bit:
        constant External_Memory_Feature_Flags := 16#00000001#;
    External_Memory_Feature_Exportable_Bit:
        constant External_Memory_Feature_Flags := 16#00000002#;
    External_Memory_Feature_Importable_Bit:
        constant External_Memory_Feature_Flags := 16#00000004#;

    type External_Fence_Handle_Type_Flags is new Flags;

    External_Fence_Handle_Type_No_Bit:
        constant External_Fence_Handle_Type_Flags := 0;
    External_Fence_Handle_Type_Opaque_FD_Bit:
        constant External_Fence_Handle_Type_Flags := 16#00000001#;
    External_Fence_Handle_Type_Opaque_Win32_Bit:
        constant External_Fence_Handle_Type_Flags := 16#00000002#;
    External_Fence_Handle_Type_Opaque_Win32_KMT_Bit:
        constant External_Fence_Handle_Type_Flags := 16#00000004#;
    External_Fence_Handle_Type_Sync_FD_Bit:
        constant External_Fence_Handle_Type_Flags := 16#00000008#;

    type External_Fence_Feature_Flags is new Flags;

    External_Fence_Feature_No_Bit: constant External_Fence_Feature_Flags := 0;
    External_Fence_Feature_Exportable_Bit:
        constant External_Fence_Feature_Flags := 16#00000001#;
    External_Fence_Feature_Importable_Bit:
        constant External_Fence_Feature_Flags := 16#00000002#;

    type Fence_Import_Flags is new Flags;

    Fence_Import_No_Bit: constant Fence_Import_Flags := 0;
    Fence_Import_Temporary_Bit: constant Fence_Import_Flags := 16#00000001#;

    type Semaphore_Import_Flags is new Flags;

    Semaphore_Import_No_Bit: constant Semaphore_Import_Flags := 0;
    Semaphore_Import_Temporary_Bit:
        constant Semaphore_Import_Flags := 16#00000001#;

    type External_Semaphore_Handle_Type_Flags is new Flags;

    External_Semaphore_Handle_Type_No_Bit:
        constant External_Semaphore_Handle_Type_Flags := 0;
    External_Semaphore_Handle_Type_Opaque_FD_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000001#;
    External_Semaphore_Handle_Type_Opaque_Win32_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000002#;
    External_Semaphore_Handle_Type_Opaque_Win32_KMT_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000004#;
    External_Semaphore_Handle_Type_D3D12_Fence_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000008#;
    External_Semaphore_Handle_Type_Sync_FD_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000010#;
    External_Semaphore_Handle_Type_Zircon_Event_Bit:
        constant External_Semaphore_Handle_Type_Flags := 16#00000020#;

    type External_Semaphore_Feature_Flags is new Flags;

    External_Semaphore_Feature_No_Bit:
        constant External_Semaphore_Feature_Flags := 0;
    External_Semaphore_Feature_Exportable_Bit:
        constant External_Semaphore_Feature_Flags := 16#00000001#;
    External_Semaphore_Feature_Importable_Bit:
        constant External_Semaphore_Feature_Flags := 16#00000002#;

    -- Vulkan 1.2
    type Resolve_Mode_Flags is new Flags;

    Resolve_Mode_No_Bit: constant Resolve_Mode_Flags := 0;
    Resolve_Mode_Sample_Zero_Bit: constant Resolve_Mode_Flags := 16#00000001#;
    Resolve_Mode_Average_Bit: constant Resolve_Mode_Flags := 16#00000002#;
    Resolve_Mode_Min_Bit: constant Resolve_Mode_Flags := 16#00000004#;
    Resolve_Mode_Max_Bit: constant Resolve_Mode_Flags := 16#00000008#;

    type Descriptor_Binding_Flags is new Flags;

    Descriptor_Binding_No_Bit: constant Descriptor_Binding_Flags := 0;
    Descriptor_Binding_Update_After_Bind_Bit:
        constant Descriptor_Binding_Flags := 16#00000001#;
    Descriptor_Binding_Update_Unused_While_Pending_Bit:
        constant Descriptor_Binding_Flags := 16#00000002#;
    Descriptor_Binding_Partially_Unbound_Bit:
        constant Descriptor_Binding_Flags := 16#00000004#;
    Descriptor_Binding_Variable_Descriptor_Count_Bit:
        constant Descriptor_Binding_Flags := 16#00000008#;

    type Semaphore_Wait_Flags is new Flags;

    Semaphore_Wait_No_Bit: constant Semaphore_Wait_Flags := 0;
    Semaphore_Wait_Any_Bit: constant Semaphore_Wait_Flags := 16#00000001#;

    -- Vulkan 1.3
    type Pipeline_Creation_Feedback_Flags is new Flags;

    Pipeline_Creation_Feedback_No_Bit:
        constant Pipeline_Creation_Feedback_Flags := 0;
    Pipeline_Creation_Feeback_Valid_Bit:
        constant Pipeline_Creation_Feedback_Flags := 16#00000001#;
    Pipeline_Creation_Feedback_Application_Pipeline_Cache_Hit_Bit:
        constant Pipeline_Creation_Feedback_Flags := 16#00000002#;
    Pipeline_Creation_Feedback_Base_Pipeline_Acceleration_Bit:
        constant Pipeline_Creation_Feedback_Flags := 16#00000004#;

    type Tool_Purpose_Flags is new Flags;

    Tool_Purpose_No_Bit: constant Tool_Purpose_Flags := 0;
    Tool_Purpose_Validation_Bit: constant Tool_Purpose_Flags := 16#00000001#;
    Tool_Purpose_Profiling_Bit: constant Tool_Purpose_Flags := 16#00000002#;
    Tool_Purpose_Tracing_Bit: constant Tool_Purpose_Flags := 16#00000004#;
    Tool_Purpose_Additional_Features_Bit:
        constant Tool_Purpose_Flags := 16#00000008#;
    Tool_Purpose_Modifying_Features_Bit:
        constant Tool_Purpose_Flags := 16#00000010#;
    Tool_Purpose_Debug_Reporting_Bit:
        constant Tool_Purpose_Flags := 16#00000020#;
    Tool_Purpose_Debug_Markers_Bit: constant Tool_Purpose_Flags := 16#00000040#;

    type Private_Data_Slot_Create_Flags is new Flags;

    Private_Data_Slot_Create_No_Bit:
        constant Private_Data_Slot_Create_Flags := 0;

    type Pipeline_Stage_Flags_2 is new Flags_64;

    Pipeline_Stage_2_None: constant Pipeline_Stage_Flags_2 := 0;
    Pipeline_Stage_2_Top_Of_Pipe_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000001#;
    Pipeline_Stage_2_Draw_Indirect_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000002#;
    Pipeline_Stage_2_Vertex_Input_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000004#;
    Pipeline_Stage_2_Vertex_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000008#;
    Pipeline_Stage_2_Tessellation_Control_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000010#;
    Pipeline_Stage_2_Tessellation_Evaluation_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000020#;
    Pipeline_Stage_2_Geometry_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000040#;
    Pipeline_Stage_2_Fragment_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000080#;
    Pipeline_Stage_2_Early_Fragment_Tests_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000100#;
    Pipeline_Stage_2_Late_Fragment_Tests_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000200#;
    Pipeline_Stage_2_Color_Attachment_Output_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000400#;
    Pipeline_Stage_2_Compute_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00000800#;
    Pipeline_Stage_2_All_Transfer_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00001000#;
    Pipeline_Stage_2_Transfer_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00001000#;
    Pipeline_Stage_2_Bottom_Of_Pipe_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00002000#;
    Pipeline_Stage_2_Host_Bit: constant Pipeline_Stage_Flags_2 := 16#00004000#;
    Pipeline_Stage_2_All_Graphics_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00008000#;
    Pipeline_Stage_2_All_Commands_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00010000#;
    Pipeline_Stage_2_Copy_Bit: constant Pipeline_Stage_Flags_2 := 16#100000000#;
    Pipeline_Stage_2_Resolve_Bit:
        constant Pipeline_Stage_Flags_2 := 16#200000000#;
    Pipeline_Stage_2_Blit_Bit: constant Pipeline_Stage_Flags_2 := 16#400000000#;
    Pipeline_Stage_2_Clear_Bit:
        constant Pipeline_Stage_Flags_2 := 16#800000000#;
    Pipeline_Stage_2_Index_Input_Bit:
        constant Pipeline_Stage_Flags_2 := 16#1000000000#;
    Pipeline_Stage_2_Vertex_Attribute_Input_Bit:
        constant Pipeline_Stage_Flags_2 := 16#2000000000#;
    Pipeline_Stage_2_Pre_Rasterization_Shaders_Bit:
        constant Pipeline_Stage_Flags_2 := 16#4000000000#;
    Pipeline_Stage_2_Video_Decode_Bit:
        constant Pipeline_Stage_Flags_2 := 16#04000000#;
    Pipeline_Stage_2_Transform_Feedback_Bit:
        constant Pipeline_Stage_Flags_2 := 16#01000000#;
    Pipeline_Stage_2_Conditional_Rendering_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00040000#;
    Pipeline_Stage_2_Command_Preprocess_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00020000#;
    Pipeline_Stage_2_Fragment_Shading_Rate_Attachment_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00400000#;
    Pipeline_Stage_2_Shading_Rate_Image_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00400000#;
    Pipeline_Stage_2_Acceleration_Structure_Build_Bit:
        constant Pipeline_Stage_Flags_2 := 16#02000000#;
    Pipeline_Stage_2_Ray_Tracing_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00200000#;
    Pipeline_Stage_2_Fragment_Density_Process_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00800000#;
    Pipeline_Stage_2_Task_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00080000#;
    Pipeline_Stage_2_Mesh_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#00100000#;
    Pipeline_Stage_2_Subpass_Shading_Bit:
        constant Pipeline_Stage_Flags_2 := 16#8000000000#;
    Pipeline_Stage_2_Invocation_Mask_Bit:
        constant Pipeline_Stage_Flags_2 := 16#10000000000#;
    Pipeline_Stage_2_Acceleration_Structure_Copy_Bit:
        constant Pipeline_Stage_Flags_2 := 16#10000000#;
    Pipeline_Stage_2_Micromap_Build_Bit:
        constant Pipeline_Stage_Flags_2 := 16#40000000#;
    Pipeline_Stage_2_Cluster_Culling_Shader_Bit:
        constant Pipeline_Stage_Flags_2 := 16#20000000000#;
    Pipeline_Stage_2_Optical_Flow_Bit:
        constant Pipeline_Stage_Flags_2 := 16#20000000#;

    type Access_Flags_2 is new Flags_64;

    Access_2_None: constant Access_Flags_2 := 0;
    Access_2_Indirect_Command_Read_Bit: constant Access_Flags_2 := 16#00000001#;
    Access_2_Index_Read_Bit: constant Access_Flags_2 := 16#00000002#;
    Access_2_Vertex_Attribute_Read_Bit: constant Access_Flags_2 := 16#00000004#;
    Access_2_Uniform_Read_Bit: constant Access_Flags_2 := 16#00000008#;
    Access_2_Input_Attachment_Read_Bit: constant Access_Flags_2 := 16#00000010#;
    Access_2_Shader_Read_Bit: constant Access_Flags_2 := 16#00000020#;
    Access_2_Shader_Write_Bit: constant Access_Flags_2 := 16#00000040#;
    Access_2_Color_Attachment_Read_Bit: constant Access_Flags_2 := 16#00000080#;
    Access_2_Color_Attachment_Write_Bit:
        constant Access_Flags_2 := 16#00000100#;
    Access_2_Depth_Stencil_Attachment_Read_Bit:
        constant Access_Flags_2 := 16#00000200#;
    Access_2_Depth_Stencil_Attachment_Write_Bit:
        constant Access_Flags_2 := 16#00000400#;
    Access_2_Transfer_Read_Bit: constant Access_Flags_2 := 16#00000800#;
    Access_2_Transfer_Write_Bit: constant Access_Flags_2 := 16#00001000#;
    Access_2_Host_Read_Bit: constant Access_Flags_2 := 16#00002000#;
    Access_2_Host_Write_Bit: constant Access_Flags_2 := 16#00004000#;
    Access_2_Memory_Read_Bit: constant Access_Flags_2 := 16#00008000#;
    Access_2_Memory_Write_Bit: constant Access_Flags_2 := 16#00010000#;
    Access_2_Shader_Sampled_Read_Bit: constant Access_Flags_2 := 16#100000000#;
    Access_2_Shader_Storage_Read_Bit: constant Access_Flags_2 := 16#200000000#;
    Access_2_Shader_Storage_Write_Bit: constant Access_Flags_2 := 16#400000000#;
    Access_2_Video_Decode_Read_Bit: constant Access_Flags_2 := 16#800000000#;
    Access_2_Video_Decode_Write_Bit: constant Access_Flags_2 := 16#1000000000#;
    Access_2_Transform_Feedback_Write_Bit:
        constant Access_Flags_2 := 16#02000000#;
    Access_2_Transform_Feedback_Counter_Read_Bit:
        constant Access_Flags_2 := 16#04000000#;
    Access_2_Transform_Feedback_Counter_Write_Bit:
        constant Access_Flags_2 := 16#08000000#;
    Access_2_Conditional_Rendering_Read_Bit:
        constant Access_Flags_2 := 16#00100000#;
    Access_2_Command_Preprocess_Read_Bit:
        constant Access_Flags_2 := 16#00020000#;
    Access_2_Command_Preprocess_Write_Bit:
        constant Access_Flags_2 := 16#00040000#;
    Access_2_Fragment_Shading_Rate_Attachment_Read_Bit:
        constant Access_Flags_2 := 16#00800000#;
    Access_2_Shading_Rate_Image_Read_Bit:
        constant Access_Flags_2 := 16#00800000#;
    Access_2_Acceleration_Structure_Read_Bit:
        constant Access_Flags_2 := 16#00200000#;
    Access_2_Acceleration_Structure_Write_Bit:
        constant Access_Flags_2 := 16#00400000#;
    Access_2_Fragment_Density_Map_Read_Bit:
        constant Access_Flags_2 := 16#01000000#;
    Access_2_Color_Attachment_Read_Noncoherent_Bit:
        constant Access_Flags_2 := 16#00080000#;
    Access_2_Descriptor_Buffer_Read_Bit:
        constant Access_Flags_2 := 16#20000000000#;
    Access_2_Invocation_Mask_Read_Bit:
        constant Access_Flags_2 := 16#8000000000#;
    Access_2_Shader_Binding_Table_Read_Bit:
        constant Access_Flags_2 := 16#10000000000#;
    Access_2_Shader_Micromap_Read_Bit:
        constant Access_Flags_2 := 16#100000000000#;
    Access_2_Shader_Micromap_Write_Bit:
        constant Access_Flags_2 := 16#200000000000#;
    Access_2_Shader_Optical_Flow_Read_Bit:
        constant Access_Flags_2 := 16#40000000000#;
    Access_2_Shader_Optical_Flow_Write_Bit:
        constant Access_Flags_2 := 16#80000000000#;

    type Submit_Flags is new Flags;

    Submit_No_Bit: constant Submit_Flags := 0;
    Submit_Protected_Bit: constant Submit_Flags := 16#00000001#;

    type Rendering_Flags is new Flags;

    Rendering_No_Bit: constant Rendering_Flags := 0;
    Rendering_Contents_Secondary_Command_Buffers_Bit:
        constant Rendering_Flags := 16#00000001#;
    Rendering_Suspending_Bit: constant Rendering_Flags := 16#00000002#;
    Rendering_Resuming_Bit: constant Rendering_Flags := 16#00000004#;
    Rendering_Enable_Legacy_Dithering_Bit:
        constant Rendering_Flags := 16#00000008#;

    type Format_Feature_Flags_2 is new Flags_64;

    Format_Feature_2_No_Bit: constant Format_Feature_Flags_2 := 0;
    Format_Feature_2_Sampled_Image_Bit:
        constant Format_Feature_Flags_2 := 16#00000001#;
    Format_Feature_2_Storage_Image_Bit:
        constant Format_Feature_Flags_2 := 16#00000002#;
    Format_Feature_2_Storage_Image_Atomic_Bit:
        constant Format_Feature_Flags_2 := 16#00000004#;
    Format_Feature_2_Uniform_Texel_Buffer_Bit:
        constant Format_Feature_Flags_2 := 16#00000008#;
    Format_Feature_2_Storage_Texel_Buffer_Bit:
        constant Format_Feature_Flags_2 := 16#00000010#;
    Format_Feature_2_Storage_Texel_Buffer_Atomic_Bit:
        constant Format_Feature_Flags_2 := 16#00000020#;
    Format_Feature_2_Vertex_Buffer_Bit:
        constant Format_Feature_Flags_2 := 16#00000040#;
    Format_Feature_2_Color_Attachment_Bit:
        constant Format_Feature_Flags_2 := 16#00000080#;
    Format_Feature_2_Color_Attachment_Blend_Bit:
        constant Format_Feature_Flags_2 := 16#00000100#;
    Format_Feature_2_Depth_Stencil_Attachment_Bit:
        constant Format_Feature_Flags_2 := 16#00000200#;
    Format_Feature_2_Blit_Src_Bit:
        constant Format_Feature_Flags_2 := 16#00000400#;
    Format_Feature_2_Blit_Dst_Bit:
        constant Format_Feature_Flags_2 := 16#00000800#;
    Format_Feature_2_Sampled_Image_Filter_Linear_Bit:
        constant Format_Feature_Flags_2 := 16#00001000#;
    Format_Feature_2_Sampled_Image_Filter_Cubic_Bit:
        constant Format_Feature_Flags_2 := 16#00002000#;
    Format_Feature_2_Transfer_Src_Bit:
        constant Format_Feature_Flags_2 := 16#00004000#;
    Format_Feature_2_Transfer_Dst_Bit:
        constant Format_Feature_Flags_2 := 16#00008000#;
    Format_Feature_2_Sampled_Image_Filter_Minmax_Bit:
        constant Format_Feature_Flags_2 := 16#00010000#;
    Format_Feature_2_Midpoint_Chroma_Samples_Bit:
        constant Format_Feature_Flags_2 := 16#00020000#;
    Format_Feature_2_Sampled_Image_YCbCr_Conversion_Linear_Filter_Bit:
        constant Format_Feature_Flags_2 := 16#00040000#;
    Format_Feature_2_Sampled_Image_YCbCr_Conversion_Separate_Reconstruction_Filter_Bit:
        constant Format_Feature_Flags_2 := 16#00080000#;
    Format_Feature_2_Sampled_Image_YCbCr_Conversion_Chroma_Reconstruction_Explicit_Bit:
        constant Format_Feature_Flags_2 := 16#00100000#;
    Format_Feature_2_Sampled_Image_YCbCr_Conversion_Chroma_Reconstruction_Explicit_Forceable_Bit:
        constant Format_Feature_Flags_2 := 16#00200000#;
    Format_Feature_2_Disjoint_Bit:
        constant Format_Feature_Flags_2 := 16#00400000#;
    Format_Feature_2_Cosited_Chroma_Samples_Bit:
        constant Format_Feature_Flags_2 := 16#00800000#;
    Format_Feature_2_Storage_Read_Without_Format_Bit:
        constant Format_Feature_Flags_2 := 16#80000000#;
    Format_Feature_2_Storage_Write_Without_Format_Bit:
        constant Format_Feature_Flags_2 := 16#100000000#;
    Format_Feature_2_Sampled_Image_Depth_Comparison_Bit:
        constant Format_Feature_Flags_2 := 16#200000000#;
    Format_Feature_2_Video_Decode_Output_Bit:
        constant Format_Feature_Flags_2 := 16#02000000#;
    Format_Feature_2_Video_Decode_DPB_Bit:
        constant Format_Feature_Flags_2 := 16#04000000#;
    Format_Feature_2_Acceleration_Structure_Vertex_Buffer_Bit:
        constant Format_Feature_Flags_2 := 16#20000000#;
    Format_Feature_2_Fragment_Density_Map_Bit:
        constant Format_Feature_Flags_2 := 16#01000000#;
    Format_Feature_2_Fragment_Shading_Rate_Attachment_Bit:
        constant Format_Feature_Flags_2 := 16#40000000#;
    Format_Feature_2_Linear_Color_Attachment_Bit:
        constant Format_Feature_Flags_2 := 16#4000000000#;
    Format_Feature_2_Weight_Image_Bit:
        constant Format_Feature_Flags_2 := 16#400000000#;
    Format_Feature_2_Weight_Sampled_Image_Bit:
        constant Format_Feature_Flags_2 := 16#800000000#;
    Format_Feature_2_Block_Matching_Bit:
        constant Format_Feature_Flags_2 := 16#1000000000#;
    Format_Feature_2_Box_Filter_Sampled_Bit:
        constant Format_Feature_Flags_2 := 16#2000000000#;
    Format_Feature_2_Optical_Flow_Image_Bit:
        constant Format_Feature_Flags_2 := 16#10000000000#;
    Format_Feature_2_Optical_Flow_Vector_Bit:
        constant Format_Feature_Flags_2 := 16#20000000000#;
    Format_Feature_2_Optical_Flow_Cost_Bit:
        constant Format_Feature_Flags_2 := 16#40000000000#;
 
    -- Vulkan 1.4
    type Memory_Unmap_Flags is new Flags;

    Memory_Unmap_No_Bit: constant Memory_Unmap_Flags := 0;
    Memory_Unmap_Reserve_Bit: constant Memory_Unmap_Flags := 16#00000001#;

    type Pipeline_Create_Flags_2 is new Flags_64;
    
    Pipeline_Create_2_No_Bit: constant Pipeline_Create_Flags_2 := 0;
    Pipeline_Create_2_Disable_Optimization_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000001#;
    Pipeline_Create_2_Allow_Derivatives_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000002#;
    Pipeline_Create_2_Derivative_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000004#;
    Pipeline_Create_2_View_Index_From_Device_Index_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000008#;
    Pipeline_Create_2_Dispatch_Base_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000010#;
    Pipeline_Create_2_Defer_Compile_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000020#;
    Pipeline_Create_2_Capture_Statistics_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000040#;
    Pipeline_Create_2_Capture_Internal_Representations_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000080#;
    Pipeline_Create_2_Fail_On_Pipeline_Compile_Required_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000100#;
    Pipeline_Create_2_Early_Return_On_Failure_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000200#;
    Pipeline_Create_2_Link_Time_Optimization_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000400#;
    Pipeline_Create_2_Library_Bit:
        constant Pipeline_Create_Flags_2 := 16#00000800#;
    Pipeline_Create_2_Ray_Tracing_Skip_Triangles_Bit:
        constant Pipeline_Create_Flags_2 := 16#00001000#;
    Pipeline_Create_2_Ray_Tracing_Skip_AABBs_Bit:
        constant Pipeline_Create_Flags_2 := 16#00002000#;
    Pipeline_Create_2_Ray_Tracing_No_Null_Any_Hit_Shaders_Bit:
        constant Pipeline_Create_Flags_2 := 16#00004000#;
    Pipeline_Create_2_Ray_Tracing_No_Null_Closest_Hit_Shaders_Bit:
        constant Pipeline_Create_Flags_2 := 16#00008000#;
    Pipeline_Create_2_Ray_Tracing_No_Null_Miss_Shaders_Bit:
        constant Pipeline_Create_Flags_2 := 16#00010000#;
    Pipeline_Create_2_Ray_Tracing_No_Null_Intersection_Shaders_Bit:
        constant Pipeline_Create_Flags_2 := 16#00020000#;
    Pipeline_Create_2_Indirect_Bindable_Bit:
        constant Pipeline_Create_Flags_2 := 16#00040000#;
    Pipeline_Create_2_Ray_Tracing_Shader_Group_Handle_Capture_Replay_Bit:
        constant Pipeline_Create_Flags_2 := 16#00080000#;
    Pipeline_Create_2_Ray_Tracing_Allow_Motion_Bit:
        constant Pipeline_Create_Flags_2 := 16#00100000#;
    Pipeline_Create_2_Rendering_Fragment_Shading_Rate_Attachment_Bit:
        constant Pipeline_Create_Flags_2 := 16#00200000#;
    Pipeline_Create_2_Rendering_Fragment_Density_Map_Attachment_Bit:
        constant Pipeline_Create_Flags_2 := 16#00400000#;
    Pipeline_Create_2_Retain_Link_Time_Optimization_Info_Bit:
        constant Pipeline_Create_Flags_2 := 16#00800000#;
    Pipeline_Create_2_Ray_Tracing_Opacity_Micromap_Bit:
        constant Pipeline_Create_Flags_2 := 16#01000000#;
    Pipeline_Create_2_Color_Attachment_Feedback_Loop_Bit:
        constant Pipeline_Create_Flags_2 := 16#02000000#;
    Pipeline_Create_2_Depth_Stencil_Attachment_Feedback_Loop_Bit:
        constant Pipeline_Create_Flags_2 := 16#04000000#;
    Pipeline_Create_2_No_Protected_Access_Bit:
        constant Pipeline_Create_Flags_2 := 16#08000000#;
    Pipeline_Create_2_Ray_Tracing_Displacement_Micromap_Bit:
        constant Pipeline_Create_Flags_2 := 16#10000000#;
    Pipeline_Create_2_Descriptor_Buffer_Bit:
        constant Pipeline_Create_Flags_2 := 16#20000000#;
    Pipeline_Create_2_Protected_Access_Only_Bit:
        constant Pipeline_Create_Flags_2 := 16#40000000#;
    Pipeline_Create_2_Capture_Data_Bit:
        constant Pipeline_Create_Flags_2 := 16#80000000#;
    Pipeline_Create_2_Execution_Graph_Bit:
        constant Pipeline_Create_Flags_2 := 16#100000000#;
    Pipeline_Create_2_Enable_Legacy_Dithering_Bit:
        constant Pipeline_Create_Flags_2 := 16#400000000#;

    type Buffer_Usage_Flags_2 is new Flags_64;

    Buffer_Usage_2_No_Bit: constant Buffer_Usage_Flags_2 := 0;
    Buffer_Usage_2_Transfer_Src_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000001#;
    Buffer_Usage_2_Transfer_Dst_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000002#;
    Buffer_Usage_2_Uniform_Texel_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000004#;
    Buffer_Usage_2_Storage_Texel_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000008#;
    Buffer_Usage_2_Uniform_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000010#;
    Buffer_Usage_2_Storage_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000020#;
    Buffer_Usage_2_Index_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000040#;
    Buffer_Usage_2_Vertex_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000080#;
    Buffer_Usage_2_Indirect_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000100#;
    Buffer_Usage_2_Conditional_Rendering_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000200#;
    Buffer_Usage_2_Shader_Binding_Table_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000400#;
    Buffer_Usage_2_Ray_Tracing_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000400#;
    Buffer_Usage_2_Transform_Feedback_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00000800#;
    Buffer_Usage_2_Transform_Feedback_Counter_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00001000#;
    Buffer_Usage_2_Video_Decode_Src_Bit:
        constant Buffer_Usage_Flags_2 := 16#00002000#;
    Buffer_Usage_2_Video_Decode_Dst_Bit:
        constant Buffer_Usage_Flags_2 := 16#00004000#;
    Buffer_Usage_2_Video_Encode_Dst_Bit:
        constant Buffer_Usage_Flags_2 := 16#00008000#;
    Buffer_Usage_2_Video_Encode_Src_Bit:
        constant Buffer_Usage_Flags_2 := 16#00010000#;
    Buffer_Usage_2_Shader_Device_Address_Bit:
        constant Buffer_Usage_Flags_2 := 16#00020000#;
    Buffer_Usage_2_Acceleration_Structure_Build_Input_Read_Only_Bit:
        constant Buffer_Usage_Flags_2 := 16#00080000#;
    Buffer_Usage_2_Acceleration_Structure_Storage_Bit:
        constant Buffer_Usage_Flags_2 := 16#00100000#;
    Buffer_Usage_2_Sampler_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00200000#;
    Buffer_Usage_2_Resource_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#00400000#;
    Buffer_Usage_2_Micromap_Build_Input_Read_Only_Bit:
        constant Buffer_Usage_Flags_2 := 16#00800000#;
    Buffer_Usage_2_Micromap_Storage_Bit:
        constant Buffer_Usage_Flags_2 := 16#01000000#;
    Buffer_Usage_2_Execution_Graph_Scratch_Bit:
        constant Buffer_Usage_Flags_2 := 16#02000000#;
    Buffer_Usage_2_Push_Descriptors_Descriptor_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#04000000#;
    Buffer_Usage_2_Preprocess_Buffer_Bit:
        constant Buffer_Usage_Flags_2 := 16#80000000#;

    type Host_Image_Copy_Flags is new Flags;

    Host_Image_Copy_No_Bit: constant Host_Image_Copy_Flags := 0;
    Host_Image_Copy_Memcpy: constant Host_Image_Copy_Flags := 16#00000001#;

    -- Version number types.
    type Version_Number is new Interfaces.Unsigned_32;
    type Major_Version is mod 2 ** 7;
    type Minor_Version is mod 2 ** 10;
    type Patch_Version is mod 2 ** 12;
    type Variant is mod 2 ** 3;

    -- Version manipulation functions.
    function Create_Version(Major: in Major_Version;
                            Minor: in Minor_Version;
                            Patch: in Patch_Version := 0;
                            Variant: in Vulkan.Variant := 0)
        return Version_Number
        with Inline,
             Post => Get_Major_Version(Create_Version'Result) = Major and
                     Get_Minor_Version(Create_Version'Result) = Minor and
                     Get_Patch_Version(Create_Version'Result) = Patch and
                     Get_Variant(Create_Version'Result) = Variant;

    function Get_Major_Version(Version: in Version_Number) return Major_Version
        with Inline;

    function Get_Minor_Version(Version: in Version_Number) return Minor_Version
        with Inline;

    function Get_Patch_Version(Version: in Version_Number) return Patch_Version
        with Inline;

    function Get_Variant(Version: in Version_Number) return Variant
        with Inline;

    -- Relational operators for Version_Numbers.
    -- Variant is not considered.
    function "=" (Left, Right: in Version_Number) return Boolean;
    function "<" (Left, Right: in Version_Number) return Boolean;
    function "<=" (Left, Right: in Version_Number) return Boolean;
    function ">" (Left, Right: in Version_Number) return Boolean;
    function ">=" (Left, Right: in Version_Number) return Boolean;

    -- Version constants.
    function API_Version_1_0 return Version_Number is (Create_Version(1, 0));
    function API_Version_1_1 return Version_Number is (Create_Version(1, 1));
    function API_Version_1_2 return Version_Number is (Create_Version(1, 2));
    function API_Version_1_3 return Version_Number is (Create_Version(1, 3));
    function API_Version_1_4 return Version_Number is (Create_Version(1, 4));

    -- Base structure types.
    type In_Structure is tagged;
    type In_Structure_Access is access constant In_Structure'Class;

    type In_Structure(Record_Type: In_Structure_Type) is abstract tagged
    record
        Next: In_Structure_Access;
    end record;

    type Out_Structure is tagged;
    type Out_Structure_Access is access all Out_Structure'Class;

    type Out_Structure(Record_Type: Out_Structure_Type) is abstract tagged
    record
        Next: Out_Structure_Access;
    end record;
    
    -- String vectors.
    package String_Vectors is
        new Ada.Containers.Indefinite_Vectors(Positive, String);

    -- Records.
    type Extent_2D is
    record
        Width: Vulkan.Width;
        Height: Vulkan.Height;
    end record
        with Convention => C;

    type Extent_3D is
    record
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Depth: Vulkan.Depth;
    end record
        with Convention => C;

    type Offset_2D is
    record
        X: X_Coordinate;
        Y: Y_Coordinate;
    end record
        with Convention => C;

    type Offset_3D is
    record
        X: X_Coordinate;
        Y: Y_Coordinate;
        Z: Z_Coordinate;
    end record
        with Convention => C;

    type Rect_2D is
    record
        Offset: Offset_2D;
        Extent: Extent_2D;
    end record
        with Convention => C;

    package Rect_2D_Vectors is new Ada.Containers.Vectors(Positive, Rect_2D);

    type Buffer_Memory_Barrier is
        new In_Structure(Buffer_Memory_Barrier_Type) with
    record
        Src_Access_Mask: Access_Flags := Access_No_Bit;
        Dst_Access_Mask: Access_Flags := Access_No_Bit;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Size: Device_Size;
    end record;

    package Buffer_Memory_Barrier_Vectors is
        new Ada.Containers.Vectors(Positive, Buffer_Memory_Barrier);
 
    type Dispatch_Indirect_Command is
    record
        X: X_Coordinate;
        Y: Y_Coordinate;
        Z: Z_Coordinate;
    end record
        with Convention => C;

    type Draw_Indexed_Indirect_Command is
    record
        Index_Count: Interfaces.Unsigned_32;
        Instance_Count: Interfaces.Unsigned_32;
        First_Index: Interfaces.Unsigned_32;
        Vertex_Offset: Interfaces.Integer_32;
        First_Instance: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Draw_Indirect_Command is
    record
        Vertex_Count: Interfaces.Unsigned_32;
        Instance_Count: Interfaces.Unsigned_32;
        First_Vertex: Interfaces.Unsigned_32;
        First_Instance: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Image_Subresource_Range is
    record
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
        Base_Mip_Level: Interfaces.Unsigned_32;
        Level_Count: Interfaces.Unsigned_32;
        Base_Array_Layer: Interfaces.Unsigned_32;
        Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Image_Subresource_Range_Vectors is
        new Ada.Containers.Vectors(Positive, Image_Subresource_Range);

    type Image_Memory_Barrier is
        new In_Structure(Image_Memory_Barrier_Type) with
    record
        Src_Access_Mask: Access_Flags := Access_No_Bit;
        Dst_Access_Mask: Access_Flags := Access_No_Bit;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Image: Vulkan.Image;
        Subresource_Range: Image_Subresource_Range;
    end record;

    package Image_Memory_Barrier_Vectors is
        new Ada.Containers.Vectors(Positive, Image_Memory_Barrier);

    type Memory_Barrier is new In_Structure(Memory_Barrier_Type) with
    record
        Src_Access_Mask: Access_Flags := Access_No_Bit;
        Dst_Access_Mask: Access_Flags := Access_No_Bit;
    end record;

    package Memory_Barrier_Vectors is
        new Ada.Containers.Vectors(Positive, Memory_Barrier);

    type Pipeline_Cache_Header_Version_One is
    record
        Header_Size: Interfaces.Unsigned_32;
        Header_Version: Pipeline_Cache_Header_Version;
        Vendor_ID: Interfaces.Unsigned_32;
        Device_ID: Interfaces.Unsigned_32;
        Pipeline_Cache_UUID: UUID;
    end record
        with Convention => C;

    type Allocation_Callbacks is
    record
        User_Data: Interfaces.C.Extensions.void_ptr;
        Allocator: 
            not null access function(Data: in Interfaces.C.Extensions.void_ptr;
                                     Size, Alignment: in Interfaces.C.size_t;
                                     Scope: in System_Allocation_Scope)
                       return Interfaces.C.Extensions.void_ptr
                        with Convention => C;
        Reallocator:
            not null access 
                function(Data,
                         Original: in Interfaces.C.Extensions.void_ptr;
                         Size, Alignment: in Interfaces.C.size_t;
                         Scope: in System_Allocation_Scope)
                       return Interfaces.C.Extensions.void_ptr
                        with Convention => C;
        Free: not null access
                procedure(Data, Memory: in Interfaces.C.Extensions.void_ptr)
                    with Convention => C;
        Allocation_Notifier:
            access procedure(Data: in Interfaces.C.Extensions.void_ptr;
                             Size: in Interfaces.C.size_t;
                             Allocation_Type: in Internal_Allocation_Type;
                             Scope: in System_Allocation_Scope)
                with Convention => C;
        Free_Notifier:
            access procedure(Data: in Interfaces.C.Extensions.void_ptr;
                             Size: in Interfaces.C.size_t;
                             Allocation_Type: in Internal_Allocation_Type;
                             Scope: in System_Allocation_Scope)
                with Convention => C;
    end record
        with Convention => C;

    type Application_Info is new In_Structure(Application_Info_Type) with
    record
        Application_Name: Ada.Strings.Unbounded.Unbounded_String;
        Application_Version: Version_Number;
        Engine_Name: Ada.Strings.Unbounded.Unbounded_String;
        Engine_Version: Version_Number;
        API_Version: Version_Number;
    end record;

    type Application_Info_Access is access constant Application_Info
        with Storage_Size => 0;

    type Format_Properties is
    record
        Linear_Tiling_Feature: Format_Feature_Flags := Format_Feature_No_Bit;
        Optimal_Tiling_Features: Format_Feature_Flags := Format_Feature_No_Bit;
        Buffer_Features: Format_Feature_Flags := Format_Feature_No_Bit;
    end record
        with Convention => C;

    type Image_Format_Properties is
    record
        Max_Extent: Extent_3D;
        Max_Mip_Levels: Mip_Levels;
        Max_Array_Layers: Array_Layers;
        Sample_Counts: Sample_Count_Flags := Sample_Count_No_Bit;
        Max_Resource_Size: Device_Size;
    end record
        with Convention => C;

    type Instance_Create_Info is
        new In_Structure(Instance_Create_Info_Type) with
    record
        Flags: Instance_Create_Flags := Instance_Create_No_Bit;
        Application_Info: Application_Info_Access;
        Enabled_Layer_Names: String_Vectors.Vector;
        Enabled_Extension_Names: String_Vectors.Vector;
    end record;

    type Memory_Heap is
    record
        Size: Device_Size;
        Flags: Memory_Heap_Flags := Memory_Heap_No_Bit;
    end record
        with Convention => C;

    package Memory_Heap_Vectors is
        new Ada.Containers.Vectors(Positive, Memory_Heap);

    type Memory_Type is
    record
        Flags: Memory_Property_Flags := Memory_Property_No_Bit;
        Heap_Index: Interfaces.Unsigned_32;
    end record
        with Convention => C;
   
    package Memory_Type_Vectors is
        new Ada.Containers.Vectors(Positive, Memory_Type);

    type Physical_Device_Features is
    record
        Robust_Buffer_Access: Boolean;
        Full_Draw_Index_Uint32: Boolean;
        Image_Cube_Array: Boolean;
        Independent_Blend: Boolean;
        Geometry_Shader: Boolean;
        Tessellation_Shader: Boolean;
        Sample_Rate_Shading: Boolean;
        Dual_Src_Blend: Boolean;
        Logic_Op: Boolean;
        Multi_Draw_Indirect: Boolean;
        Draw_Indirect_First_Instance: Boolean;
        Depth_Clamp: Boolean;
        Depth_Bias_Clamp: Boolean;
        Fill_Mode_Non_Solid: Boolean;
        Depth_Bounds: Boolean;
        Wide_Lines: Boolean;
        Large_Points: Boolean;
        Alpha_To_One: Boolean;
        Multi_Viewport: Boolean;
        Sampler_Anisotropy: Boolean;
        Texture_Compression_ETC2: Boolean;
        Texture_Compression_ASTC_LDR: Boolean;
        Texture_Compression_BC: Boolean;
        Occlusion_Query_Precise: Boolean;
        Pipeline_Statistics_Query: Boolean;
        Vertex_Pipeline_Stores_And_Atomics: Boolean;
        Fragment_Stores_And_Atomics: Boolean;
        Shader_Tessellation_And_Geometry_Point_Size: Boolean;
        Shader_Image_Gather_Extended: Boolean;
        Shader_Storage_Image_Extended_Formats: Boolean;
        Shader_Storage_Image_Multisample: Boolean;
        Shader_Storage_Image_Read_Without_Format: Boolean;
        Shader_Storage_Image_Write_Without_Format: Boolean;
        Shader_Uniform_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Sampled_Image_Array_Dynamic_Indexing: Boolean;
        Shader_Storage_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Storage_Image_Array_Dynamic_Indexing: Boolean;
        Shader_Clip_Distance: Boolean;
        Shader_Cull_Distance: Boolean;
        Shader_Float64: Boolean;
        Shader_Int64: Boolean;
        Shader_Int16: Boolean;
        Shader_Resource_Residency: Boolean;
        Shader_Resource_Min_Lod: Boolean;
        Sparse_Binding: Boolean;
        Sparse_Residency_Buffer: Boolean;
        Sparse_Residency_Image_2D: Boolean;
        Sparse_Residency_Image_3D: Boolean;
        Sparse_Residency_2_Samples: Boolean;
        Sparse_Residency_4_Samples: Boolean;
        Sparse_Residency_8_Samples: Boolean;
        Sparse_Residency_16_Samples: Boolean;
        Sparse_Residency_Aliased: Boolean;
        Variable_Multisample_Rate: Boolean;
        Inherited_Queries: Boolean;
    end record;

    type Physical_Device_Features_Access is
        access constant Physical_Device_Features
        with Storage_Size => 0;
  
    type Work_Group_Array is array (1 .. 3) of Interfaces.Unsigned_32
        with Convention => C;
    type Viewport_Dimensions is array (1 .. 2) of Interfaces.Unsigned_32
        with Convention => C;
    type Float_Range is array (1 .. 2) of Float;

    type Physical_Device_Limits is
    record
        Max_Image_Dimension_1D: Interfaces.Unsigned_32;
        Max_Image_Dimension_2D: Interfaces.Unsigned_32;
        Max_Image_Dimension_3D: Interfaces.Unsigned_32;
        Max_Image_Dimension_Cube: Interfaces.Unsigned_32;
        Max_Image_Array_Layers: Array_Layers;
        Max_Texel_Buffer_Elements: Interfaces.Unsigned_32;
        Max_Uniform_Buffer_Range: Interfaces.Unsigned_32;
        Max_Storage_Buffer_Range: Interfaces.Unsigned_32;
        Max_Push_Constants_Size: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Count: Interfaces.Unsigned_32;
        Max_Sampler_Allocation_Count: Interfaces.Unsigned_32;
        Buffer_Image_Granularity: Device_Size;
        Sparse_Address_Space_Size: Device_Size;
        Max_Bound_Descriptor_Sets: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Samplers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Uniform_Buffers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Storage_Buffers: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Sampled_Images: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Storage_Images: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Input_Attachments: Interfaces.Unsigned_32;
        Max_Per_Stage_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Uniform_Buffers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Uniform_Buffers_Dynamic: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Buffers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Buffers_Dynamic: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Sampled_Images: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Storage_Images: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Input_Attachments: Interfaces.Unsigned_32;
        Max_Vertex_Input_Attributes: Interfaces.Unsigned_32;
        Max_Vertex_Input_Bindings: Interfaces.Unsigned_32;
        Max_Vertex_Input_Attribute_Offset: Interfaces.Unsigned_32;
        Max_Vertex_Input_Binding_Stride: Interfaces.Unsigned_32;
        Max_Vertex_Output_Components: Interfaces.Unsigned_32;
        Max_Tessellation_Generation_Level: Interfaces.Unsigned_32;
        Max_Tessellation_Patch_Size: Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Vertex_Input_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Vertex_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Per_Patch_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Control_Total_Output_Components:
            Interfaces.Unsigned_32;
        Max_Tessellation_Evaluation_Input_Components: Interfaces.Unsigned_32;
        Max_Tessellation_Evaluation_Output_Components: Interfaces.Unsigned_32;
        Max_Geometry_Shader_Invocations: Interfaces.Unsigned_32;
        Max_Geometry_Input_Components: Interfaces.Unsigned_32;
        Max_Geometry_Output_Components: Interfaces.Unsigned_32;
        Max_Geometry_Output_Vertices: Interfaces.Unsigned_32;
        Max_Geometry_Total_Output_Components: Interfaces.Unsigned_32;
        Max_Fragment_Input_Components: Interfaces.Unsigned_32;
        Max_Fragment_Output_Attachments: Interfaces.Unsigned_32;
        Max_Fragment_Dual_Src_Attachments: Interfaces.Unsigned_32;
        Max_Fragment_Combined_Output_Resources: Interfaces.Unsigned_32;
        Max_Compute_Shared_Memory_Size: Interfaces.Unsigned_32;
        Max_Compute_Work_Group_Count: Work_Group_Array;
        Max_Compute_Work_Group_Invocations: Interfaces.Unsigned_32;
        Max_Compute_Work_Group_Size: Work_Group_Array;
        Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
        Sub_Texel_Precision_Bits: Interfaces.Unsigned_32;
        Mipmap_Precision_Bits: Interfaces.Unsigned_32;
        Max_Draw_Indexed_Index_Value: Interfaces.Unsigned_32;
        Max_Draw_Indirect_Count: Interfaces.Unsigned_32;
        Max_Sampler_Lod_Bias: Float;
        Max_Sampler_Anisotropy: Float;
        Max_Viewports: Interfaces.Unsigned_32;
        Max_Viewport_Dimensions: Viewport_Dimensions;
        Viewport_Bounds_Range: Float_Range;
        Viewport_Sub_Pixel_Bits: Interfaces.Unsigned_32;
        Min_Memory_Map_Alignment: Interfaces.C.size_t;
        Min_Texel_Buffer_Offset_Alignment: Device_Size;
        Min_Uniform_Buffer_Offset_Alignment: Device_Size;
        Min_Storage_Buffer_Offset_Alignment: Device_Size;
        Min_Texel_Offset: Interfaces.Integer_32;
        Max_Texel_Offset: Interfaces.Unsigned_32;
        Min_Texel_Gather_Offset: Interfaces.Integer_32;
        Max_Texel_Gather_Offset: Interfaces.Unsigned_32;
        Min_Interpolation_Offset: Float;
        Max_Interpolation_Offset: Float;
        Sub_Pixel_Interpolation_Offset_Bits: Interfaces.Unsigned_32;
        Max_Framebuffer_Width: Interfaces.Unsigned_32;
        Max_Framebuffer_Height: Interfaces.Unsigned_32;
        Max_Framebuffer_Layers: Interfaces.Unsigned_32;
        Framebuffer_Color_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Framebuffer_Depth_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Framebuffer_Stencil_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Framebuffer_No_Attachments_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Max_Color_Attachments: Interfaces.Unsigned_32;
        Sampled_Image_Color_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Sampled_Image_Integer_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Sampled_Image_Depth_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Sampled_Image_Stencil_Sample_Counts:
            Sample_Count_Flags := Sample_Count_No_Bit;
        Storage_Image_Sample_Counts: Sample_Count_Flags := Sample_Count_No_Bit;
        Max_Sample_Mask_Words: Interfaces.Unsigned_32;
        Timestamp_Compute_And_Graphics: Boolean;
        Timestamp_Period: Float;
        Max_Clip_Distances: Interfaces.Unsigned_32;
        Max_Cull_Distances: Interfaces.Unsigned_32;
        Max_Combined_Clip_And_Cull_Distances: Interfaces.Unsigned_32;
        Discrete_Queue_Priorities: Interfaces.Unsigned_32;
        Point_Size_Range: Float_Range;
        Line_Width_Range: Float_Range;
        Point_Size_Granularity: Float;
        Line_Width_Granularity: Float;
        Strict_Lines: Boolean;
        Standard_Sample_Locations: Boolean;
        Optimal_Buffer_Copy_Offset_Alignment: Device_Size;
        Optimal_Buffer_Copy_Row_Pitch_Alignment: Device_Size;
        Non_Coherent_Atom_Size: Device_Size;
    end record;

    type Physical_Device_Memory_Properties is
    record
        Memory_Types: Memory_Type_Vectors.Vector;
        Memory_Heaps: Memory_Heap_Vectors.Vector;
    end record;

    type Physical_Device_Sparse_Properties is
    record
        Residency_Standard_2D_Block_Shape: Boolean;
        Residency_Standard_2D_Multisample_Block_Shape: Boolean;
        Residency_Standard_3D_Block_Shape: Boolean;
        Residency_Aligned_Mip_Size: Boolean;
        Residency_Non_Resident_Strict: Boolean;
    end record;

    type Physical_Device_Properties is
    record
        API_Version: Version_Number;
        Driver_Version: Version_Number;
        Vendor_ID: Interfaces.Unsigned_32;
        Device_ID: Interfaces.Unsigned_32;
        Device_Type: Physical_Device_Type;
        Device_Name: Ada.Strings.Unbounded.Unbounded_String;
        Pipeline_Cache_UUID: UUID;
        Limits: Physical_Device_Limits;
        Sparse_Properties: Physical_Device_Sparse_Properties;
    end record;

    type Queue_Family_Properties is
    record
        Flags: Queue_Flags := Queue_No_Bit;
        Count: Interfaces.Unsigned_32;
        Timestamp_Valid_Bits: Interfaces.Unsigned_32;
        Min_Image_Transfer_Granularity: Extent_3D;
    end record
        with Convention => C;

    package Queue_Family_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Queue_Family_Properties);
 
    package Queue_Priority_Vectors is
        new Ada.Containers.Vectors(Positive, Float);

    type Device_Queue_Create_Info is
        new In_Structure(Device_Queue_Create_Info_Type) with
    record
        Flags: Device_Queue_Create_Flags := Device_Queue_Create_No_Bit;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Priorities: Queue_Priority_Vectors.Vector;
    end record;

    package Device_Queue_Create_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Device_Queue_Create_Info);

    type Device_Create_Info is
        new In_Structure(Device_Create_Info_Type) with
    record
        Flags: Device_Create_Flags := Device_Create_No_Bit;
        Queue_Create_Infos: Device_Queue_Create_Info_Vectors.Vector;
        Enabled_Layer_Names: String_Vectors.Vector;
        Enabled_Extension_Names: String_Vectors.Vector;
        Enabled_Features: Physical_Device_Features_Access;
    end record;

    type Extension_Properties is
    record
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Spec_Version: Version_Number;
    end record;

    type Extension_Properties_Access is access constant Extension_Properties
        with Storage_Size => 0;

    package Extension_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Extension_Properties);

    type Layer_Properties is
    record
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Spec_Version: Version_Number;
        Implementation_Version: Version_Number;
        Description: Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Layer_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Layer_Properties);

    package Pipeline_Stage_Flags_Vectors is
        new Ada.Containers.Vectors(Positive, Pipeline_Stage_Flags);
    
    type Submit_Info is new In_Structure(Submit_Info_Type) with
    record
        Wait_Semaphores: Semaphore_Vectors.Vector;
        Wait_Dst_Stage_Mask: Pipeline_Stage_Flags_Vectors.Vector;
        Command_Buffers: Command_Buffer_Vectors.Vector;
        Signal_Semaphores: Semaphore_Vectors.Vector;
    end record;

    package Submit_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Submit_Info);

    type Mapped_Memory_Range is new In_Structure(Mapped_Memory_Range_Type) with
    record
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record;

    package Mapped_Memory_Range_Vectors is
        new Ada.Containers.Vectors(Positive, Mapped_Memory_Range);

    type Memory_Allocate_Info is
        new In_Structure(Memory_Allocate_Info_Type) with
    record
        Allocation_Size: Device_Size;
        Memory_Type_Index: Interfaces.Unsigned_32;
    end record;
    
    type Memory_Requirements is
    record
        Size: Device_Size;
        Alignment: Device_Size;
        Memory_Type_Bits: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sparse_Memory_Bind is
    record
        Resource_Offset: Device_Size;
        Size: Device_Size;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
        Flags: Sparse_Memory_Bind_Flags := Sparse_Memory_Bind_No_Bit;
    end record
        with Convention => C;

    package Sparse_Memory_Bind_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Memory_Bind);

    type Sparse_Buffer_Memory_Bind_Info is
    record
        Buffer: Vulkan.Buffer;
        Binds: Sparse_Memory_Bind_Vectors.Vector;
    end record;

    package Sparse_Buffer_Memory_Bind_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Buffer_Memory_Bind_Info);

    type Sparse_Image_Opaque_Memory_Bind_Info is
    record
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
        Binds: Sparse_Memory_Bind_Vectors.Vector;
    end record;

    package Sparse_Image_Opaque_Memory_Bind_Info_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Sparse_Image_Opaque_Memory_Bind_Info);

    type Image_Subresource is
    record
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
        Mip_Level: Interfaces.Unsigned_32;
        Array_Layer: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Sparse_Image_Memory_Bind is
    record
        Subresource: Image_Subresource;
        Offset: Offset_3D;
        Extent: Extent_3D;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
        Flags: Sparse_Memory_Bind_Flags := Sparse_Memory_Bind_No_Bit;
    end record
        with Convention => C;

    package Sparse_Image_Memory_Bind_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Image_Memory_Bind);

    type Sparse_Image_Memory_Bind_Info is
    record
        Image: Vulkan.Image;
        Binds: Sparse_Image_Memory_Bind_Vectors.Vector;
    end record;

    package Sparse_Image_Memory_Bind_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Image_Memory_Bind_Info);
 
    type Bind_Sparse_Info is new In_Structure(Bind_Sparse_Info_Type) with
    record
        Wait_Semaphores: Semaphore_Vectors.Vector;
        Buffer_Binds: Sparse_Buffer_Memory_Bind_Info_Vectors.Vector;
        Image_Opaque_Binds: Sparse_Image_Opaque_Memory_Bind_Info_Vectors.Vector;
        Image_Binds: Sparse_Image_Memory_Bind_Info_Vectors.Vector;
        Signal_Semaphores: Semaphore_Vectors.Vector;
    end record;

    package Bind_Sparse_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Bind_Sparse_Info);

    type Sparse_Image_Format_Properties is
    record
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
        Image_Granularity: Extent_3D;
        Flags: Sparse_Image_Format_Flags := Sparse_Image_Format_No_Bit;
    end record
        with Convention => C;

    package Sparse_Image_Format_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Image_Format_Properties);

    type Sparse_Image_Memory_Requirements is
    record
        Format_Properties: Sparse_Image_Format_Properties;
        Mip_Tail_First_Lod: Interfaces.Unsigned_32;
        Mip_Tail_Size: Device_Size;
        Mip_Tail_Offset: Device_Size;
        Mip_Tail_Stride: Device_Size;
    end record
        with Convention => C;

    package Sparse_Image_Memory_Requirements_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Image_Memory_Requirements); 
      
    type Fence_Create_Info is new In_Structure(Fence_Create_Info_Type) with
    record
        Flags: Fence_Create_Flags := Fence_Create_No_Bit;
    end record;

    type Semaphore_Create_Info is
        new In_Structure(Semaphore_Create_Info_Type) with
    record
        Flags: Semaphore_Create_Flags := Semaphore_Create_No_Bit;
    end record;

    type Event_Create_Info is new In_Structure(Event_Create_Info_Type) with
    record
        Flags: Event_Create_Flags := Event_Create_No_Bit;
    end record;

    type Query_Pool_Create_Info is
        new In_Structure(Query_Pool_Create_Info_Type) with
    record
        Flags: Query_Pool_Create_Flags := Query_Pool_Create_No_Bit;
        Query_Type: Vulkan.Query_Type;
        Query_Count: Interfaces.Unsigned_32;
        Pipeline_Statistics:
            Query_Pipeline_Statistic_Flags := Query_Pipeline_Statistic_No_Bit;
    end record;

    package Queue_Family_Index_Vectors is
        new Ada.Containers.Vectors(Positive, Queue_Family_Index);

    type Buffer_Create_Info is new In_Structure(Buffer_Create_Info_Type) with
    record
        Flags: Buffer_Create_Flags := Buffer_Create_No_Bit;
        Size: Device_Size;
        Usage: Buffer_Usage_Flags := Buffer_Usage_No_Bit;
        Sharing_Mode: Vulkan.Sharing_Mode;
        Queue_Family_Indices: Queue_Family_Index_Vectors.Vector;
    end record;

    type Buffer_Create_Info_Access is access constant Buffer_Create_Info
        with Storage_Size => 0;

    type Buffer_View_Create_Info is
        new In_Structure(Buffer_View_Create_Info_Type) with
    record
        Flags: Buffer_View_Create_Flags := Buffer_View_Create_No_Bit;
        Buffer: Vulkan.Buffer;
        Format: Vulkan.Format;
        Offset: Device_Size;
        View_Range: Device_Size;
    end record;

    type Image_Create_Info is
        new In_Structure(Image_Create_Info_Type) with
    record
        Flags: Image_Create_Flags := Image_Create_No_Bit;
        Image_Type: Vulkan.Image_Type;
        Format: Vulkan.Format;
        Extent: Extent_3D;
        Mip_Levels: Positive;
        Array_Layers: Positive;
        Samples: Sample_Count_Flags := Sample_Count_No_Bit;
        Tiling: Image_Tiling;
        Usage: Image_Usage_Flags := Image_Usage_No_Bit;
        Sharing_Mode: Vulkan.Sharing_Mode;
        Queue_Family_Indices: Queue_Family_Index_Vectors.Vector;
        Initial_Layout: Image_Layout;
    end record;

    type Image_Create_Info_Access is access constant Image_Create_Info
        with Storage_Size => 0;

    type Subresource_Layout is
    record
        Offset: Device_Size;
        Size: Device_Size;
        Row_Pitch: Device_Size;
        Array_Pitch: Device_Size;
        Depth_Pitch: Device_Size;
    end record
        with Convention => C;

    type Component_Mapping is
    record
        R: Component_Swizzle;
        G: Component_Swizzle;
        B: Component_Swizzle;
        A: Component_Swizzle;
    end record
        with Convention => C;
 
    type Image_View_Create_Info is
        new In_Structure(Image_View_Create_Info_Type) with
    record
        Flags: Image_View_Create_Flags := Image_View_Create_No_Bit;
        Image: Vulkan.Image;
        View_Type: Image_View_Type;
        Format: Vulkan.Format;
        Components: Component_Mapping;
        Subresource_Range: Image_Subresource_Range;
    end record;

    package Unsigned_32_Vectors is 
        new Ada.Containers.Vectors(Positive,
                                   Interfaces.Unsigned_32,
                                   Interfaces."=");
    
    type Shader_Module_Create_Info is
        new In_Structure(Shader_Module_Create_Info_Type) with
    record
        Flags: Shader_Module_Create_Flags := Shader_Module_Create_No_Bit;
        Code: Unsigned_32_Vectors.Vector;
    end record;

    type Pipeline_Cache_Create_Info is
        new In_Structure(Pipeline_Cache_Create_Info_Type) with
    record
        Flags: Pipeline_Cache_Create_Flags;
        Initial_Data_Size: Interfaces.C.size_t;
        Initial_Data: Interfaces.C.Extensions.void_ptr;
    end record;
    
    type Specialization_Map_Entry is
    record
        Constant_ID: Interfaces.Unsigned_32;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.C.size_t;
    end record
        with Convention => C;

    package Specialization_Map_Entry_Vectors is
        new Ada.Containers.Vectors(Positive, Specialization_Map_Entry);

    type Specialization_Info is
    record
        Map_Entries: Specialization_Map_Entry_Vectors.Vector;
        Data_Size: Interfaces.C.size_t;
        Data: Interfaces.C.Extensions.void_ptr;
    end record;

    type Constant_Specialization_Info_Access is
        access constant Specialization_Info
        with Storage_Size => 0;

    type Pipeline_Shader_Stage_Create_Info is
        new In_Structure(Pipeline_Shader_Stage_Create_Info_Type) with
    record
        Flags: Pipeline_Shader_Stage_Create_Flags :=
                Pipeline_Shader_Stage_Create_No_Bit;
        Stage: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Module: Shader_Module;
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Specialization_Info: Constant_Specialization_Info_Access;
    end record;

    package Pipeline_Shader_Stage_Create_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Pipeline_Shader_Stage_Create_Info);

    type Compute_Pipeline_Create_Info is
        new In_Structure(Compute_Pipeline_Create_Info_Type) with
    record
        Flags: Pipeline_Create_Flags := Pipeline_Create_No_Bit;
        Stage: Pipeline_Shader_Stage_Create_Info;
        Layout: Pipeline_Layout;
        Base_Pipeline_Handle: Pipeline;
        Base_Pipeline_Index: Interfaces.Unsigned_32;
    end record;

    package Compute_Pipeline_Create_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Compute_Pipeline_Create_Info);

    type Vertex_Input_Binding_Description is
    record
        Binding: Interfaces.Unsigned_32;
        Stride: Interfaces.Unsigned_32;
        Input_Rate: Vertex_Input_Rate;
    end record
        with Convention => C;

    package Vertex_Input_Binding_Description_Vectors is
        new Ada.Containers.Vectors(Positive, Vertex_Input_Binding_Description);

    type Vertex_Input_Attribute_Description is
    record
        Location: Interfaces.Unsigned_32;
        Binding: Interfaces.Unsigned_32;
        Format: Vulkan.Format;
        Offset: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Vertex_Input_Attribute_Description_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Vertex_Input_Attribute_Description);

    type Pipeline_Vertex_Input_State_Create_Info is
        new In_Structure(Pipeline_Vertex_Input_State_Create_Info_Type) with
    record
        Flags: Pipeline_Vertex_Input_State_Create_Flags :=
            Pipeline_Vertex_Input_State_Create_No_Bit;
        Vertex_Binding_Descriptions:
            Vertex_Input_Binding_Description_Vectors.Vector;
        Vertex_Attribute_Descriptions:
            Vertex_Input_Attribute_Description_Vectors.Vector;
    end record;

    type Pipeline_Vertex_Input_State_Create_Info_Access is
        access constant Pipeline_Vertex_Input_State_Create_Info
        with Storage_Size => 0;

    type Pipeline_Input_Assembly_State_Create_Info is
        new In_Structure(Pipeline_Input_Assembly_State_Create_Info_Type) with
    record
        Flags: Pipeline_Input_Assembly_State_Create_Flags :=
            Pipeline_Input_Assembly_State_Create_No_Bit;
        Topology: Primitive_Topology;
        Primitive_Restart_Enable: Boolean;
    end record;

    type Pipeline_Input_Assembly_State_Create_Info_Access is
        access constant Pipeline_Input_Assembly_State_Create_Info
        with Storage_Size => 0;

    type Pipeline_Tessellation_State_Create_Info is
        new In_Structure(Pipeline_Tessellation_State_Create_Info_Type) with
    record
        Flags: Pipeline_Tessellation_State_Create_Flags :=
            Pipeline_Tessellation_State_Create_No_Bit;
        Patch_Control_Points: Interfaces.Unsigned_32;
    end record;

    type Pipeline_Tessellation_State_Create_Info_Access is
        access constant Pipeline_Tessellation_State_Create_Info
        with Storage_Size => 0;

    type Viewport is
    record
        X: Interfaces.C.C_Float;
        Y: Interfaces.C.C_Float;
        Width: Interfaces.C.C_Float;
        Height: Interfaces.C.C_Float;
        Min_Depth: Interfaces.C.C_Float;
        Max_Depth: Interfaces.C.C_Float;
    end record
        with Convention => C;
 
    package Viewport_Vectors is
        new Ada.Containers.Vectors(Positive, Viewport);

    type Pipeline_Viewport_State_Create_Info is
        new In_Structure(Pipeline_Viewport_State_Create_Info_Type) with
    record
        Flags: Pipeline_Viewport_State_Create_Flags :=
            Pipeline_Viewport_State_Create_No_Bit;
        Viewports: Viewport_Vectors.Vector;
        Scissors: Rect_2D_Vectors.Vector;
    end record;

    type Pipeline_Viewport_State_Create_Info_Access is
        access constant Pipeline_Viewport_State_Create_Info
        with Storage_Size => 0;

    type Pipeline_Rasterization_State_Create_Info is
        new In_Structure(Pipeline_Rasterization_State_Create_Info_Type) with
    record
        Flags: Pipeline_Rasterization_State_Create_Flags :=
            Pipeline_Rasterization_State_Create_No_Bit;
        Depth_Clamp_Enable: Boolean;
        Rasterizer_Discard_Enable: Boolean;
        Polygon_Mode: Vulkan.Polygon_Mode;
        Cull_Mode: Cull_Mode_Flags := Cull_Mode_None;
        Front_Face: Vulkan.Front_Face;
        Depth_Bias_Enable: Boolean;
        Depth_Bias_Constant_Factor: Interfaces.C.C_Float;
        Depth_Bias_Clamp: Interfaces.C.C_Float;
        Depth_Bias_Slope_Factor: Interfaces.C.C_Float;
        Line_Width: Interfaces.C.C_Float;
    end record;

    type Pipeline_Rasterization_State_Create_Info_Access is
        access constant Pipeline_Rasterization_State_Create_Info
        with Storage_Size => 0;
    
    package Sample_Mask_Vectors is
        new Ada.Containers.Vectors(Positive, Sample_Mask);

    type Pipeline_Multisample_State_Create_Info is
        new In_Structure(Pipeline_Multisample_State_Create_Info_Type) with
    record
        Flags: Pipeline_Multisample_State_Create_Flags :=
            Pipeline_Multisample_State_Create_No_Bit;
        Rasterization_Samples: Sample_Count_Flags := Sample_Count_No_Bit;
        Sample_Shading_Enable: Boolean;
        Min_Sample_Shading: Interfaces.C.C_Float;
        Sample_Masks: Sample_Mask_Vectors.Vector;
        Alpha_To_Coverage_Enable: Boolean;
        Alpha_To_One_Enable: Boolean;
    end record;
    
    type Pipeline_Multisample_State_Create_Info_Access is
        access constant Pipeline_Multisample_State_Create_Info
        with Storage_Size => 0;

    type Stencil_Op_State is
    record
        Fail_Op: Stencil_Op;
        Pass_Op: Stencil_Op;
        Depth_Fail_Op: Stencil_Op;
        Compare_Op: Vulkan.Compare_Op;
        Compare_Mask: Interfaces.Unsigned_32;
        Write_Mask: Interfaces.Unsigned_32;
        Reference: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Depth_Stencil_State_Create_Info is
        new In_Structure(Pipeline_Depth_Stencil_State_Create_Info_Type) with
    record
        Flags: Pipeline_Depth_Stencil_State_Create_Flags :=
            Pipeline_Depth_Stencil_State_Create_No_Bit;
        Depth_Test_Enable: Boolean;
        Depth_Write_Enable: Boolean;
        Depth_Compare_Op: Compare_Op;
        Depth_Bounds_Test_Enable: Boolean;
        Stencil_Test_Enable: Boolean;
        Front: Stencil_Op_State;
        Back: Stencil_Op_State;
        Min_Depth_Bounds: Interfaces.C.C_Float;
        Max_Depth_Bounds: Interfaces.C.C_Float;
    end record;

    type Pipeline_Depth_Stencil_State_Create_Info_Access is
        access constant Pipeline_Depth_Stencil_State_Create_Info
        with Storage_Size => 0;

    type Pipeline_Color_Blend_Attachment_State is
    record
        Blend_Enable: Boolean;
        Src_Color_Blend_Factor: Blend_Factor;
        Dst_Color_Blend_Factor: Blend_Factor;
        Color_Blend_Op: Blend_Op;
        Src_Alpha_Blend_Factor: Blend_Factor;
        Dst_Alpha_Blend_Factor: Blend_Factor;
        Alpha_Blend_Op: Blend_Op;
        Color_Write_Mask: Color_Component_Flags := Color_Component_No_Bit;
    end record;

    package Pipeline_Color_Blend_Attachment_State_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Pipeline_Color_Blend_Attachment_State);

    type Blend_Constants_Array is array (1 .. 4)
        of aliased Interfaces.C.C_Float
        with Convention => C;

    type Pipeline_Color_Blend_State_Create_Info is
        new In_Structure(Pipeline_Color_Blend_State_Create_Info_Type) with
    record
        Flags: Pipeline_Color_Blend_State_Create_Flags :=
            Pipeline_Color_Blend_State_Create_No_Bit;
        Logic_Op_Enable: Boolean;
        Logic_Op: Vulkan.Logic_Op;
        Attachments: Pipeline_Color_Blend_Attachment_State_Vectors.Vector;
        Blend_Constants: Blend_Constants_Array;
    end record;

    type Pipeline_Color_Blend_State_Create_Info_Access is
        access constant Pipeline_Color_Blend_State_Create_Info
        with Storage_Size => 0;

    package Dynamic_State_Vectors is
        new Ada.Containers.Vectors(Positive, Dynamic_State);

    type Pipeline_Dynamic_State_Create_Info is
        new In_Structure(Pipeline_Dynamic_State_Create_Info_Type) with
    record
        Flags: Pipeline_Dynamic_State_Create_Flags :=
            Pipeline_Dynamic_State_Create_No_Bit;
        Dynamic_States: Dynamic_State_Vectors.Vector;
    end record;

    type Pipeline_Dynamic_State_Create_Info_Access is
        access constant Pipeline_Dynamic_State_Create_Info
        with Storage_Size => 0;

    type Graphics_Pipeline_Create_Info is
        new In_Structure(Graphics_Pipeline_Create_Info_Type) with
    record
        Flags: Pipeline_Create_Flags := Pipeline_Create_No_Bit;
        Stages: Pipeline_Shader_Stage_Create_Info_Vectors.Vector;
        Vertex_Input_State: Pipeline_Vertex_Input_State_Create_Info_Access;
        Input_Assembly_State: Pipeline_Input_Assembly_State_Create_Info_Access;
        Tessellation_State: Pipeline_Tessellation_State_Create_Info_Access;
        Viewport_State: Pipeline_Viewport_State_Create_Info_Access;
        Rasterization_State: Pipeline_Rasterization_State_Create_Info_Access;
        Multisample_State: Pipeline_Multisample_State_Create_Info_Access;
        Depth_Stencil_State: Pipeline_Depth_Stencil_State_Create_Info_Access;
        Color_Blend_State: Pipeline_Color_Blend_State_Create_Info_Access;
        Dynamic_State: Pipeline_Dynamic_State_Create_Info_Access;
        Layout: Pipeline_Layout;
        Render_Pass: Vulkan.Render_Pass;
        Subpass: Interfaces.Unsigned_32;
        Base_Pipeline_Handle: Pipeline;
        Base_Pipeline_Index: Interfaces.Integer_32;
    end record;

    package Graphics_Pipeline_Create_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Graphics_Pipeline_Create_Info);
 
    type Push_Constant_Range is
    record
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Push_Constant_Range_Vectors is
        new Ada.Containers.Vectors(Positive, Push_Constant_Range);

    type Pipeline_Layout_Create_Info is
        new In_Structure(Pipeline_Layout_Create_Info_Type) with
    record
        Flags: Pipeline_Layout_Create_Flags := Pipeline_Layout_Create_No_Bit;
        Set_Layouts: Descriptor_Set_Layout_Vectors.Vector;
        Push_Constant_Ranges: Push_Constant_Range_Vectors.Vector;
    end record;

    type Sampler_Create_Info is new In_Structure(Sampler_Create_Info_Type) with
    record
        Flags: Sampler_Create_Flags := Sampler_Create_No_Bit;
        Mag_Filter: Filter;
        Min_Filter: Filter;
        Mipmap_Mode: Sampler_Mipmap_Mode;
        Address_Mode_U: Sampler_Address_Mode;
        Address_Mode_V: Sampler_Address_Mode;
        Address_Mode_W: Sampler_Address_Mode;
        Mip_Lod_Bias: Interfaces.C.C_Float;
        Anisotropy_Enable: Boolean;
        Max_Anisotropy: Interfaces.C.C_Float;
        Compare_Enable: Boolean;
        Compare_Op: Vulkan.Compare_Op;
        Min_Lod: Interfaces.C.C_Float;
        Max_Lod: Interfaces.C.C_Float;
        Border_Color: Vulkan.Border_Color;
        Unnormalized_Coordinates: Boolean;
    end record;

    type Copy_Descriptor_Set is new In_Structure(Copy_Descriptor_Set_Type) with
    record
        Src_Set: Descriptor_Set;
        Src_Binding: Interfaces.Unsigned_32;
        Src_Array_Element: Interfaces.Unsigned_32;
        Dst_Set: Descriptor_Set;
        Dst_Binding: Interfaces.Unsigned_32;
        Dst_Array_Element: Interfaces.Unsigned_32;
        Descriptor_Count: Interfaces.Unsigned_32;
    end record;

    package Copy_Descriptor_Set_Vectors is
        new Ada.Containers.Vectors(Positive, Copy_Descriptor_Set);

    type Descriptor_Buffer_Info is
    record
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Buffer_Range: Device_Size;
    end record
        with Convention => C;

    package Descriptor_Buffer_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Buffer_Info);

    type Descriptor_Image_Info is
    record
        Sampler: Vulkan.Sampler;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
    end record
        with Convention => C;
 
    package Descriptor_Image_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Image_Info);

    type Descriptor_Pool_Size is
    record
        Descriptor_Type: Vulkan.Descriptor_Type;
        Descriptor_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Descriptor_Pool_Size_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Pool_Size);

    type Descriptor_Pool_Create_Info is
        new In_Structure(Descriptor_Pool_Create_Info_Type) with
    record
        Flags: Descriptor_Pool_Create_Flags := Descriptor_Pool_Create_No_Bit;
        Max_Sets: Interfaces.Unsigned_32;
        Pool_Sizes: Descriptor_Pool_Size_Vectors.Vector;
    end record;

    type Descriptor_Set_Allocate_Info is
        new In_Structure(Descriptor_Set_Allocate_Info_Type) with
    record
        Descriptor_Pool: Vulkan.Descriptor_Pool;
        Set_Layouts: Descriptor_Set_Layout_Vectors.Vector;
    end record;

    type Descriptor_Set_Layout_Binding is
    record
        Binding: Interfaces.Unsigned_32;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Descriptor_Count: Interfaces.Unsigned_32;
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Immutable_Samplers: Sampler_Vectors.Vector;
    end record;

    package Descriptor_Set_Layout_Binding_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Set_Layout_Binding);

    type Descriptor_Set_Layout_Create_Info is
        new In_Structure(Descriptor_Set_Layout_Create_Info_Type) with
    record
        Flags: Descriptor_Set_Layout_Create_Flags :=
                Descriptor_Set_Layout_Create_No_Bit;
        Bindings: Descriptor_Set_Layout_Binding_Vectors.Vector;
    end record;
       
    type Write_Descriptor_Set is
        new In_Structure(Write_Descriptor_Set_Type) with
    record
        Dst_Set: Descriptor_Set;
        Dst_Binding: Interfaces.Unsigned_32;
        Dst_Array_Element: Interfaces.Unsigned_32;
        Descriptor_Count: Interfaces.Unsigned_32;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Image_Info: Descriptor_Image_Info_Vectors.Vector;
        Buffer_Info: Descriptor_Buffer_Info_Vectors.Vector;
        Texel_Buffer_View: Buffer_View_Vectors.Vector;
    end record;

    package Write_Descriptor_Set_Vectors is
        new Ada.Containers.Vectors(Positive, Write_Descriptor_Set);
 
    type Attachment_Description is
    record
        Flags: Attachment_Description_Flags := Attachment_Description_No_Bit;
        Format: Vulkan.Format;
        Samples: Sample_Count_Flags := Sample_Count_No_Bit;
        Load_Op: Attachment_Load_Op;
        Store_Op: Attachment_Store_Op;
        Stencil_Load_Op: Attachment_Load_Op;
        Stencil_Store_Op: Attachment_Store_Op;
        Initial_Layout: Image_Layout;
        Final_Layout: Image_Layout;
    end record
        with Convention => C;

    package Attachment_Description_Vectors is
        new Ada.Containers.Vectors(Positive, Attachment_Description);

    type Attachment_Reference is
    record
        Attachment: Interfaces.Unsigned_32;
        Layout: Image_Layout;
    end record
        with Convention => C;

    type Attachment_Reference_Access is access constant Attachment_Reference
        with Convention => C,
             Storage_Size => 0;

    package Attachment_Reference_Vectors is
        new Ada.Containers.Vectors(Positive, Attachment_Reference);

    type Framebuffer_Create_Info is
        new In_Structure(Framebuffer_Create_Info_Type) with
    record
        Flags: Framebuffer_Create_Flags := Framebuffer_Create_No_Bit;
        Render_Pass: Vulkan.Render_Pass;
        Attachments: Image_View_Vectors.Vector;
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Layers: Interfaces.Unsigned_32;
    end record;
  
    type Subpass_Description is
    record
        Flags: Subpass_Description_Flags := Subpass_Description_No_Bit;
        Input_Attachments: Attachment_Reference_Vectors.Vector;
        Color_Attachments: Attachment_Reference_Vectors.Vector;
        Resolve_Attachments: Attachment_Reference_Vectors.Vector;
        Depth_Stencil_Attachment: Attachment_Reference_Access;
        Preserve_Attachments: Unsigned_32_Vectors.Vector;
    end record;

    package Subpass_Description_Vectors is
        new Ada.Containers.Vectors(Positive, Subpass_Description);
    
    type Subpass_Dependency is
    record
        Src_Subpass: Interfaces.Unsigned_32;
        Dst_Subpass: Interfaces.Unsigned_32;
        Src_Stage_Mask: Pipeline_Stage_Flags := Pipeline_Stage_No_Bit;
        Dst_Stage_Mask: Pipeline_Stage_Flags := Pipeline_Stage_No_Bit;
        Src_Access_Mask: Access_Flags := Access_No_Bit;
        Dst_Access_Mask: Access_Flags := Access_No_Bit;
        Dependency_Flags: Vulkan.Dependency_Flags := Dependency_No_Bit;
    end record
        with Convention => C;

    package Subpass_Dependency_Vectors is
        new Ada.Containers.Vectors(Positive, Subpass_Dependency);

    type Render_Pass_Create_Info is
        new In_Structure(Render_Pass_Create_Info_Type) with
    record
        Flags: Render_Pass_Create_Flags := Render_Pass_Create_No_Bit;
        Attachments: Attachment_Description_Vectors.Vector;
        Subpasses: Subpass_Description_Vectors.Vector;
        Dependencies: Subpass_Dependency_Vectors.Vector;
    end record;

    type Command_Pool_Create_Info is
        new In_Structure(Command_Pool_Create_Info_Type) with
    record
        Flags: Command_Pool_Create_Flags := Command_Pool_Create_No_Bit;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
    end record;

    type Command_Buffer_Allocate_Info is
        new In_Structure(Command_Buffer_Allocate_Info_Type) with
    record
        Command_Pool: Vulkan.Command_Pool;
        Level: Command_Buffer_Level;
        Command_Buffer_Count: Interfaces.Unsigned_32;
    end record;

    type Command_Buffer_Inheritance_Info is
        new In_Structure(Command_Buffer_Inheritance_Info_Type) with
    record
        Render_Pass: Vulkan.Render_Pass;
        Subpass: Interfaces.Unsigned_32;
        Framebuffer: Vulkan.Framebuffer;
        Occlusion_Query_Enable: Boolean;
        Query_Flags: Query_Control_Flags := Query_Control_No_Bit;
        Pipeline_Statistics:
            Query_Pipeline_Statistic_Flags := Query_Pipeline_Statistic_No_Bit;
    end record;

    type Command_Buffer_Inheritance_Info_Access is
        access constant Command_Buffer_Inheritance_Info
        with Storage_Size => 0;

    type Command_Buffer_Begin_Info is
        new In_Structure(Command_Buffer_Begin_Info_Type) with
    record
        Flags: Command_Buffer_Usage_Flags := Command_Buffer_Usage_No_Bit;
        Inheritance_Info: Command_Buffer_Inheritance_Info_Access;
    end record;

    type Buffer_Copy is
    record
        Src_Offset: Device_Size;
        Dst_Offset: Device_Size;
        Size: Device_Size;
    end record
        with Convention => C;

    package Buffer_Copy_Vectors is
        new Ada.Containers.Vectors(Positive, Buffer_Copy);

    type Image_Subresource_Layers is
    record
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
        Mip_Level: Interfaces.Unsigned_32;
        Base_Array_Layer: Interfaces.Unsigned_32;
        Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Buffer_Image_Copy is
    record
        Buffer_Offset: Device_Size;
        Buffer_Row_Length: Interfaces.Unsigned_32;
        Buffer_Image_Height: Interfaces.Unsigned_32;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record
        with Convention => C;

    package Buffer_Image_Copy_Vectors is
        new Ada.Containers.Vectors(Positive, Buffer_Image_Copy);

    type Clear_Color_Type is (Clear_Color_Float,
                              Clear_Color_Integer,
                              Clear_Color_Unsigned);

    type Float_Clear_Color is array (1 .. 4) of Interfaces.C.C_float
        with Convention => C;
    type Integer_Clear_Color is array (1 .. 4) of Interfaces.Integer_32
        with Convention => C;
    type Unsigned_Clear_Color is array (1 .. 4) of Interfaces.Unsigned_32
        with Convention => C;

    type Clear_Color_Value(Color_Type: Clear_Color_Type) is
    record
        case Color_Type is
            when Clear_Color_Float =>
                Float_Color: Float_Clear_Color;
            when Clear_Color_Integer =>
                Integer_Color: Integer_Clear_Color;
            when Clear_Color_Unsigned =>
                Unsigned_Color: Unsigned_Clear_Color;
        end case;
    end record;

    type Clear_Depth_Stencil_Value is
    record
        Depth: Interfaces.C.C_Float;
        Stencil: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Clear_Value_Type is (Clear_Color, Clear_Depth_Stencil);

    type Clear_Value(Clear_Type: Clear_Value_Type := Clear_Color;
                     Color_Type: Clear_Color_Type := Clear_Color_Float) is
    record
        case Clear_Type is
            when Clear_Color =>
                Color: Clear_Color_Value(Color_Type);
            when Clear_Depth_Stencil =>
                Depth_Stencil: Clear_Depth_Stencil_Value;
        end case;
    end record;

    package Clear_Value_Vectors is
        new Ada.Containers.Indefinite_Vectors(Positive, Clear_Value);

    type Clear_Attachment(Clear_Type: Clear_Value_Type := Clear_Color;
                          Color_Type: Clear_Color_Type := Clear_Color_Float) is
    record
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
        Color_Attachment: Interfaces.Unsigned_32;
        Clear_Value: Vulkan.Clear_Value(Clear_Type, Color_Type);
    end record;

    package Clear_Attachment_Vectors is
        new Ada.Containers.Indefinite_Vectors(Positive, Clear_Attachment);

    type Clear_Rect is
    record
        Rect: Rect_2D;
        Base_Array_Layer: Interfaces.Unsigned_32;
        Layer_Count: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Clear_Rect_Vectors is
        new Ada.Containers.Vectors(Positive, Clear_Rect);

    type Offset_3D_Array is array (Positive range <>) of Offset_3D
        with Convention => C;

    type Image_Blit is
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offsets: Offset_3D_Array(1 .. 2);
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offsets: Offset_3D_Array(1 .. 2);
    end record
        with Convention => C;

    package Image_Blit_Vectors is
        new Ada.Containers.Vectors(Positive, Image_Blit);

    type Image_Copy is
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record
        with Convention => C;

    package Image_Copy_Vectors is
        new Ada.Containers.Vectors(Positive, Image_Copy);
   
    type Image_Resolve is
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record
        with Convention => C;

    package Image_Resolve_Vectors is
        new Ada.Containers.Vectors(Positive, Image_Resolve);
 
    type Render_Pass_Begin_Info is
        new In_Structure(Render_Pass_Begin_Info_Type) with
    record
        Render_Pass: Vulkan.Render_Pass;
        Framebuffer: Vulkan.Framebuffer;
        Render_Area: Rect_2D;
        Clear_Values: Clear_Value_Vectors.Vector;
    end record;

    -- Vulkan 1.1
    type Physical_Device_Subgroup_Properties is
        new Out_Structure(Physical_Device_Subgroup_Properties_Type) with
    record
        Subgroup_Size: Interfaces.Unsigned_32;
        Supported_Stages: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Supported_Operations: Subgroup_Feature_Flags := Subgroup_Feature_No_Bit;
        Quad_Operations_In_All_Stages: Boolean;
    end record;

    type Bind_Buffer_Memory_Info is
        new In_Structure(Bind_Buffer_Memory_Info_Type) with
    record
        Buffer: Vulkan.Buffer;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
    end record;

    package Bind_Buffer_Memory_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Bind_Buffer_Memory_Info);

    type Bind_Image_Memory_Info is
        new In_Structure(Bind_Image_Memory_Info_Type) with
    record
        Image: Vulkan.Image;
        Memory: Device_Memory;
        Memory_Offset: Device_Size;
    end record;

    package Bind_Image_Memory_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Bind_Image_Memory_Info);

    type Physical_Device_16Bit_Storage_Features is
        new Out_Structure(Physical_Device_16Bit_Storage_Features_Type) with
    record
        Storage_Buffer_16Bit_Access: Boolean;
        Uniform_And_Storage_Buffer_16Bit_Access: Boolean;
        Storage_Push_Constant_16: Boolean;
        Storage_Input_Output_16: Boolean;
    end record;

    type Memory_Dedicated_Requirements is
        new Out_Structure(Memory_Dedicated_Requirements_Type) with
    record
        Prefers_Dedicated_Allocation: Boolean;
        Requires_Dedicated_Allocation: Boolean;
    end record;

    type Memory_Dedicated_Allocate_Info is
        new In_Structure(Memory_Dedicated_Allocate_Info_Type) with
    record
        Image: Vulkan.Image;
        Buffer: Vulkan.Buffer;
    end record;

    type Memory_Allocate_Flags_Info is
        new In_Structure(Memory_Allocate_Flags_Info_Type) with
    record
        Flags: Memory_Allocate_Flags := Memory_Allocate_No_Bit;
        Device_Mask: Interfaces.Unsigned_32;
    end record;

    type Device_Group_Render_Pass_Begin_Info is
        new In_Structure(Device_Group_Render_Pass_Begin_Info_Type) with
    record
        Device_Mask: Interfaces.Unsigned_32;
        Device_Render_Areas: Rect_2D_Vectors.Vector;
    end record;

    type Device_Group_Command_Buffer_Begin_Info is
        new In_Structure(Device_Group_Command_Buffer_Begin_Info_Type) with
    record
        Device_Mask: Interfaces.Unsigned_32;
    end record;

    type Device_Group_Submit_Info is
        new In_Structure(Device_Group_Submit_Info_Type) with
    record
        Wait_Semaphore_Device_Indices: Unsigned_32_Vectors.Vector;
        Command_Buffer_Device_Masks: Unsigned_32_Vectors.Vector;
        Signal_Semaphore_Device_Indices: Unsigned_32_Vectors.Vector;
    end record;

    type Device_Group_Bind_Sparse_Info is
        new In_Structure(Device_Group_Bind_Sparse_Info_Type) with
    record
        Resource_Device_Index: Interfaces.Unsigned_32;
        Memory_Device_Index: Interfaces.Unsigned_32;
    end record;

    type Bind_Buffer_Memory_Device_Group_Info is
        new In_Structure(Bind_Buffer_Memory_Device_Group_Info_Type) with
    record
        Device_Indices: Unsigned_32_Vectors.Vector;
    end record;

    type Bind_Image_Memory_Device_Group_Info is
        new In_Structure(Bind_Image_Memory_Device_Group_Info_Type) with
    record
        Device_Indices: Unsigned_32_Vectors.Vector;
        Split_Instance_Bind_Regions: Rect_2D_Vectors.Vector;
    end record;

    type Physical_Device_Group_Properties is
        new Out_Structure(Physical_Device_Group_Properties_Type) with
    record
        Physical_Devices: Physical_Device_Vectors.Vector;
        Subset_Allocation: Boolean;
    end record;

    package Physical_Device_Group_Properties_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Physical_Device_Group_Properties);

    type Device_Group_Device_Create_Info is
        new In_Structure(Device_Group_Device_Create_Info_Type) with
    record
        Physical_Devices: Physical_Device_Vectors.Vector;
    end record;

    type Buffer_Memory_Requirements_Info_2 is
        new In_Structure(Buffer_Memory_Requirements_Info_2_Type) with
    record
        Buffer: Vulkan.Buffer;
    end record;

    type Image_Memory_Requirements_Info_2 is
        new In_Structure(Image_Memory_Requirements_Info_2_Type) with
    record
        Image: Vulkan.Image;
    end record;

    type Image_Sparse_Memory_Requirements_Info_2 is
        new In_Structure(Image_Sparse_Memory_Requirements_Info_2_Type) with
    record
        Image: Vulkan.Image;
    end record;

    type Memory_Requirements_2 is
        new Out_Structure(Memory_Requirements_2_Type) with
    record
        Memory_Requirements: Vulkan.Memory_Requirements;
    end record;

    type Sparse_Image_Memory_Requirements_2 is
        new Out_Structure(Sparse_Image_Memory_Requirements_2_Type) with
    record
        Memory_Requirements: Sparse_Image_Memory_Requirements;
    end record;

    package Sparse_Image_Memory_Requirements_2_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Sparse_Image_Memory_Requirements_2);

    type Physical_Device_Features_2 is
        new Out_Structure(Physical_Device_Features_2_Type) with
    record
        Features: Physical_Device_Features;
    end record;

    type Physical_Device_Properties_2 is
        new Out_Structure(Physical_Device_Properties_2_Type) with
    record
        Properties: Physical_Device_Properties;
    end record;

    type Format_Properties_2 is
        new Out_Structure(Format_Properties_2_Type) with
    record
        Format_Properties: Vulkan.Format_Properties;
    end record;

    type Image_Format_Properties_2 is
        new Out_Structure(Image_Format_Properties_2_Type) with
    record
        Image_Format_Properties: Vulkan.Image_Format_Properties;
    end record;

    type Physical_Device_Image_Format_Info_2 is
        new In_Structure(Physical_Device_Image_Format_Info_2_Type) with
    record
        Format: Vulkan.Format;
        Image_Type: Vulkan.Image_Type;
        Tiling: Image_Tiling;
        Usage: Image_Usage_Flags := Image_Usage_No_Bit;
        Flags: Image_Create_Flags := Image_Create_No_Bit;
    end record;

    type Queue_Family_Properties_2 is
        new Out_Structure(Queue_Family_Properties_2_Type) with
    record
        Queue_Family_Properties: Vulkan.Queue_Family_Properties;
    end record;

    package Queue_Family_Properties_2_Vectors is
        new Ada.Containers.Vectors(Positive, Queue_Family_Properties_2);

    type Physical_Device_Memory_Properties_2 is
        new Out_Structure(Physical_Device_Memory_Properties_2_Type) with
    record
        Memory_Properties: Physical_Device_Memory_Properties;
    end record;

    type Sparse_Image_Format_Properties_2 is
        new Out_Structure(Sparse_Image_Format_Properties_2_Type) with
    record
        Properties: Sparse_Image_Format_Properties;
    end record;

    package Sparse_Image_Format_Properties_2_Vectors is
        new Ada.Containers.Vectors(Positive, Sparse_Image_Format_Properties_2);

    type Physical_Device_Sparse_Image_Format_Info_2 is
        new In_Structure(Physical_Device_Sparse_Image_Format_Info_2_Type) with
    record
        Format: Vulkan.Format;
        Image_Type: Vulkan.Image_Type;
        Samples: Sample_Count_Flags := Sample_Count_No_Bit;
        Usage: Image_Usage_Flags := Image_Usage_No_Bit;
        Tiling: Image_Tiling;
    end record;
    
    type Physical_Device_Point_Clipping_Properties is
        new Out_Structure(Physical_Device_Point_Clipping_Properties_Type) with
    record
        Point_Clipping_Behavior: Vulkan.Point_Clipping_Behavior;
    end record;

    type Input_Attachment_Aspect_Reference is
    record
        Subpass: Interfaces.Unsigned_32;
        Input_Attachment_Index: Interfaces.Unsigned_32;
        Aspect_Mask: Image_Aspect_Flags := Image_Aspect_No_Bit;
    end record
        with Convention => C;

    package Input_Attachment_Aspect_Reference_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Input_Attachment_Aspect_Reference);

    type Render_Pass_Input_Attachment_Aspect_Create_Info is
        new In_Structure
            (Render_Pass_Input_Attachment_Aspect_Create_Info_Type) with
    record
        Aspect_References: Input_Attachment_Aspect_Reference_Vectors.Vector;
    end record;

    type Image_View_Usage_Create_Info is
        new In_Structure(Image_View_Usage_Create_Info_Type) with
    record
        Usage: Image_Usage_Flags := Image_Usage_No_Bit;
    end record;

    type Pipeline_Tessellation_Domain_Origin_State_Create_Info is
        new In_Structure
            (Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type) with
    record
        Domain_Origin: Tessellation_Domain_Origin;
    end record;

    type Render_Pass_Multiview_Create_Info is
        new In_Structure(Render_Pass_Multiview_Create_Info_Type) with
    record
        View_Masks: Unsigned_32_Vectors.Vector;
        View_Offsets: Unsigned_32_Vectors.Vector;
        Correlation_Masks: Unsigned_32_Vectors.Vector;
    end record;

    type Physical_Device_Multiview_Features is
        new Out_Structure(Physical_Device_Multiview_Features_Type) with
    record
        Multiview: Boolean;
        Multiview_Geometry_Shader: Boolean;
        Multiview_Tessellation_Shader: Boolean;
    end record;

    type Physical_Device_Multiview_Properties is
        new Out_Structure(Physical_Device_Multiview_Properties_Type) with
    record
        Max_Multiview_View_Count: Interfaces.Unsigned_32;
        Max_Multiview_Instance_Index: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Variable_Pointer_Features is
        new Out_Structure(Physical_Device_Variable_Pointer_Features_Type) with
    record
        Variable_Pointers_Storage_Buffer: Boolean;
        Variable_Pointers: Boolean;
    end record;

    type Physical_Device_Protected_Memory_Features is
        new Out_Structure(Physical_Device_Protected_Memory_Features_Type) with
    record
        Protected_Memory: Boolean;
    end record;

    type Physical_Device_Protected_Memory_Properties is
        new Out_Structure(Physical_Device_Protected_Memory_Properties_Type) with
    record
        Protected_No_Fault: Boolean;
    end record;

    type Device_Queue_Info_2 is
        new In_Structure(Device_Queue_Info_2_Type) with
    record
        Flags: Device_Queue_Create_Flags := Device_Queue_Create_No_Bit;
        Queue_Family_Index: Vulkan.Queue_Family_Index;
        Queue_Index: Interfaces.Unsigned_32;
    end record;

    type Protected_Submit_Info is
        new In_Structure(Protected_Submit_Info_Type) with
    record
        Protected_Submit: Boolean;
    end record;

    type Sampler_YCbCr_Conversion_Create_Info is
        new In_Structure(Sampler_YCbCr_Conversion_Create_Info_Type) with
    record
        Format: Vulkan.Format;
        YCbCr_Model: Sampler_YCbCr_Model_Conversion;
        YCbCr_Range: Sampler_YCbCr_Range;
        Components: Component_Mapping;
        X_Chroma_Offset: Chroma_Location;
        Y_Chroma_Offset: Chroma_Location;
        Chroma_Filter: Filter;
        Force_Explicit_Reconstruction: Boolean;
    end record;

    type Sampler_YCbCr_Conversion_Info is
        new In_Structure(Sampler_YCbCr_Conversion_Info_Type) with
    record
        Conversion: Sampler_YCbCr_Conversion;
    end record;

    type Bind_Image_Plane_Memory_Info is
        new In_Structure(Bind_Image_Plane_Memory_Info_Type) with
    record
        Plane_Aspect: Image_Aspect_Flags := Image_Aspect_No_Bit;
    end record;

    type Image_Plane_Memory_Requirements_Info is
        new In_Structure(Image_Plane_Memory_Requirements_Info_Type) with
    record
        Plane_Aspect: Image_Aspect_Flags := Image_Aspect_No_Bit;
    end record;

    type Physical_Device_Sampler_YCbCr_Conversion_Features is
        new Out_Structure
            (Physical_Device_Sampler_YCbCr_Conversion_Features_Type) with
    record
        Sampler_YCbCr_Conversion: Boolean;
    end record;

    type Sampler_YCbCr_Conversion_Image_Format_Properties is
        new Out_Structure
            (Sampler_YCbCr_Conversion_Image_Format_Properties_Type) with
    record
        Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
    end record;

    type Descriptor_Update_Template_Entry is
    record
        Dst_Binding: Interfaces.Unsigned_32;
        Dst_Array_Element: Interfaces.Unsigned_32;
        Descriptor_Count: Interfaces.Unsigned_32;
        Descriptor_Type: Vulkan.Descriptor_Type;
        Offset: Interfaces.C.size_t;
        Stride: Interfaces.C.size_t;
    end record
        with Convention => C;

    package Descriptor_Update_Template_Entry_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Update_Template_Entry);

    type Descriptor_Update_Template_Create_Info is
        new In_Structure(Descriptor_Update_Template_Create_Info_Type) with
    record
        Flags: Descriptor_Update_Template_Create_Flags
            := Descriptor_Update_Template_Create_No_Bit;
        Descriptor_Update_Entries:
            Descriptor_Update_Template_Entry_Vectors.Vector;
        Template_Type: Descriptor_Update_Template_Type;
        Descriptor_Set_Layout: Vulkan.Descriptor_Set_Layout;
        Pipeline_Bind_Point: Vulkan.Pipeline_Bind_Point;
        Pipeline_Layout: Vulkan.Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
    end record;

    type External_Memory_Properties is
    record
        External_Memory_Features: External_Memory_Feature_Flags
            := External_Memory_Feature_No_Bit;
        Export_From_Imported_Handle_Types: External_Memory_Handle_Type_Flags
            := External_Memory_Handle_Type_No_Bit;
        Compatible_Handle_Types: External_Memory_Handle_Type_Flags
            := External_Memory_Handle_Type_No_Bit;
    end record
        with Convention => C;

    type Physical_Device_External_Image_Format_Info is
        new In_Structure(Physical_Device_External_Image_Format_Info_Type) with
    record
        Handle_Type: External_Memory_Handle_Type_Flags
            := External_Memory_Handle_Type_No_Bit;
    end record;

    type External_Image_Format_Properties is
        new Out_Structure(External_Image_Format_Properties_Type) with
    record
        External_Memory_Properties: Vulkan.External_Memory_Properties;
    end record;

    type Physical_Device_External_Buffer_Info is
        new In_Structure(Physical_Device_External_Buffer_Info_Type) with
    record
        Flags: Buffer_Create_Flags := Buffer_Create_No_Bit;
        Usage: Buffer_Usage_Flags := Buffer_Usage_No_Bit;
        Handle_Type: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
    end record;

    type External_Buffer_Properties is
        new Out_Structure(External_Buffer_Properties_Type) with
    record
        External_Memory_Properties: Vulkan.External_Memory_Properties;
    end record;

    type Physical_Device_ID_Properties is
        new Out_Structure(Physical_Device_ID_Properties_Type) with
    record
        Device_UUID: UUID;
        Driver_UUID: UUID;
        Device_LUID: LUID;
        Device_Node_Mask: Interfaces.Unsigned_32;
        Device_LUID_Valid: Boolean;
    end record;

    type External_Memory_Image_Create_Info is
        new In_Structure(External_Memory_Image_Create_Info_Type) with
    record
        Handle_Types: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
    end record;

    type External_Memory_Buffer_Create_Info is
        new In_Structure(External_Memory_Buffer_Create_Info_Type) with
    record
        Handle_Types: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
    end record;

    type Export_Memory_Allocate_Info is
        new In_Structure(Export_Memory_Allocate_Info_Type) with
    record
        Handle_Types: External_Memory_Handle_Type_Flags :=
            External_Memory_Handle_Type_No_Bit;
    end record;

    type Physical_Device_External_Fence_Info is
        new In_Structure(Physical_Device_External_Fence_Info_Type) with
    record
        Handle_Type: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
    end record;

    type External_Fence_Properties is
        new Out_Structure(External_Fence_Properties_Type) with
    record
        Export_From_Imported_Handle_Types: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
        Compatible_Handle_Types: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
        External_Fence_Features: External_Fence_Feature_Flags :=
            External_Fence_Feature_No_Bit;
    end record;

    type Export_Fence_Create_Info is
        new In_Structure(Export_Fence_Create_Info_Type) with
    record
        Handle_Types: External_Fence_Handle_Type_Flags :=
            External_Fence_Handle_Type_No_Bit;
    end record;

    type Export_Semaphore_Create_Info is
        new In_Structure(Export_Semaphore_Create_Info_Type) with
    record
        Handle_Types: External_Semaphore_Handle_Type_Flags :=
            External_Semaphore_Handle_Type_No_Bit;
    end record;

    type Physical_Device_External_Semaphore_Info is
        new In_Structure(Physical_Device_External_Semaphore_Info_Type) with
    record
        Handle_Type: External_Semaphore_Handle_Type_Flags :=
            External_Semaphore_Handle_Type_No_Bit;
    end record;

    type External_Semaphore_Properties is
        new Out_Structure(External_Semaphore_Properties_Type) with
    record
        Export_From_Imported_Handle_Types:
            External_Semaphore_Handle_Type_Flags :=
                External_Semaphore_Handle_Type_No_Bit;
        Compatible_Handle_Types: External_Semaphore_Handle_Type_Flags :=
            External_Semaphore_Handle_Type_No_Bit;
        External_Semaphore_Features: External_Semaphore_Feature_Flags :=
            External_Semaphore_Feature_No_Bit;
    end record;

    type Physical_Device_Maintenance_3_Properties is
        new Out_Structure(Physical_Device_Maintenance_3_Properties_Type) with
    record
        Max_Per_Set_Descriptors: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Size: Device_Size;
    end record;

    type Descriptor_Set_Layout_Support is
        new Out_Structure(Descriptor_Set_Layout_Support_Type) with
    record
        Supported: Boolean;
    end record;

    type Physical_Device_Shader_Draw_Parameter_Features is
        new Out_Structure
            (Physical_Device_Shader_Draw_Parameter_Features_Type) with
    record
        Shader_Draw_Parameters: Boolean;
    end record;

    -- Vulkan 1.2
    type Physical_Device_Vulkan_1_1_Features is
        new Out_Structure(Physical_Device_Vulkan_1_1_Features_Type) with
    record
        Storage_Buffer_16Bit_Access: Boolean;
        Uniform_And_Storage_Buffer_16Bit_Access: Boolean;
        Storage_Push_Constant_16: Boolean;
        Storage_Input_Output_16: Boolean;
        Multiview: Boolean;
        Multiview_Geometry_Shader: Boolean;
        Multiview_Tessellation_Shader: Boolean;
        Variable_Pointers_Storage_Buffer: Boolean;
        Variable_Pointers: Boolean;
        Protected_Memory: Boolean;
        Sampler_YCbCr_Conversion: Boolean;
        Shader_Draw_Parameters: Boolean;
    end record;

    type Physical_Device_Vulkan_1_1_Properties is
        new Out_Structure(Physical_Device_Vulkan_1_1_Properties_Type) with
    record
        Device_UUID: UUID;
        Driver_UUID: UUID;
        Device_LUID: LUID;
        Device_Node_Mask: Interfaces.Unsigned_32;
        Device_LUID_Valid: Boolean;
        Subgroup_Size: Interfaces.Unsigned_32;
        Subgroup_Supported_Stages: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Subgroup_Supported_Operations: Subgroup_Feature_Flags :=
            Subgroup_Feature_No_Bit;
        Subgroup_Quad_Operations_In_All_Stages: Boolean;
        Point_Clipping_Behavior: Vulkan.Point_Clipping_Behavior;
        Max_Multiview_View_Count: Interfaces.Unsigned_32;
        Max_Multiview_Instance_Index: Interfaces.Unsigned_32;
        Protected_No_Fault: Boolean;
        Max_Per_Set_Descriptors: Interfaces.Unsigned_32;
        Max_Memory_Allocation_Size: Device_Size;
    end record;

    type Physical_Device_Vulkan_1_2_Features is
        new Out_Structure(Physical_Device_Vulkan_1_2_Features_Type) with
    record
        Sampler_Mirror_Clamp_To_Edge: Boolean;
        Draw_Indirect_Count: Boolean;
        Storage_Buffer_8Bit_Access: Boolean;
        Uniform_And_Storage_Buffer_8Bit_Access: Boolean;
        Storage_Push_Constant_8: Boolean;
        Shader_Buffer_Int64_Atomics: Boolean;
        Shader_Shared_Int64_Atomics: Boolean;
        Shader_Float16: Boolean;
        Shader_Int8: Boolean;
        Descriptor_Indexing: Boolean;
        Shader_Input_Attachment_Array_Dynamic_Indexing: Boolean;
        Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Image_Array_Non_Uniform_Indexing: Boolean;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing: Boolean;
        Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Descriptor_Binding_Uniform_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Sampled_Image_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Image_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Update_Unused_While_Pending: Boolean;
        Descriptor_Binding_Partially_Bound: Boolean;
        Descriptor_Binding_Variable_Descriptor_Count: Boolean;
        Runtime_Descriptor_Array: Boolean;
        Sampler_Filter_Minmax: Boolean;
        Scalar_Block_Layout: Boolean;
        Imageless_Framebuffer: Boolean;
        Uniform_Buffer_Standard_Layout: Boolean;
        Shader_Subgroup_Extended_Types: Boolean;
        Separate_Depth_Stencil_Layouts: Boolean;
        Host_Query_Reset: Boolean;
        Timeline_Semaphore: Boolean;
        Buffer_Device_Address: Boolean;
        Buffer_Device_Address_Capture_Replay: Boolean;
        Buffer_Device_Address_Multi_Device: Boolean;
        Vulkan_Memory_Model: Boolean;
        Vulkan_Memory_Model_Device_Scope: Boolean;
        Vulkan_Memory_Model_Availability_Visibility_Chains: Boolean;
        Shader_Output_Viewport_Index: Boolean;
        Shader_Output_Layer: Boolean;
        Subgroup_Broadcast_Dynamic_ID: Boolean;
    end record;

    type Conformance_Version is
    record
        Major: Interfaces.Unsigned_8;
        Minor: Interfaces.Unsigned_8;
        Subminor: Interfaces.Unsigned_8;
        Patch: Interfaces.Unsigned_8;
    end record
        with Convention => C;

    type Physical_Device_Vulkan_1_2_Properties is
        new Out_Structure(Physical_Device_Vulkan_1_2_Properties_Type) with
    record
        Driver_ID: Vulkan.Driver_ID;
        Driver_Name: Ada.Strings.Unbounded.Unbounded_String;
        Driver_Info: Ada.Strings.Unbounded.Unbounded_String;
        Conformance_Version: Vulkan.Conformance_Version;
        Denorm_Behavior_Independence: Shader_Float_Controls_Independence;
        Rounding_Mode_Independence: Shader_Float_Controls_Independence;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float16: Boolean;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float32: Boolean;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float64: Boolean;
        Shader_Denorm_Preserve_Float16: Boolean;
        Shader_Denorm_Preserve_Float32: Boolean;
        Shader_Denorm_Preserve_Float64: Boolean;
        Shader_Denorm_Flush_To_Zero_Float16: Boolean;
        Shader_Denorm_Flush_To_Zero_Float32: Boolean;
        Shader_Denorm_Flush_To_Zero_Float64: Boolean;
        Shader_Rounding_Mode_RTE_Float16: Boolean;
        Shader_Rounding_Mode_RTE_Float32: Boolean;
        Shader_Rounding_Mode_RTE_Float64: Boolean;
        Shader_Rounding_Mode_RTZ_Float16: Boolean;
        Shader_Rounding_Mode_RTZ_Float32: Boolean;
        Shader_Rounding_Mode_RTZ_Float64: Boolean;
        Max_Update_After_Bind_Descriptors_In_All_Pools: Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Storage_Image_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native: Boolean;
        Robust_Buffer_Access_Update_After_Bind: Boolean;
        Quad_Divergent_Implicit_LOD: Boolean;
        Max_Per_Stage_Descriptor_Update_After_Bind_Samplers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Update_After_Bind_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Supported_Depth_Resolve_Modes: Resolve_Mode_Flags :=
            Resolve_Mode_No_Bit;
        Supported_Stencil_Resolve_Modes: Resolve_Mode_Flags :=
            Resolve_Mode_No_Bit;
        Independent_Resolve_None: Boolean;
        Independent_Resolve: Boolean;
        Filter_Minmax_Single_Component_Formats: Boolean;
        Filter_Minmax_Image_Component_Mapping: Boolean;
        Max_Timeline_Semaphore_Value_Difference: Semaphore_Value;
        Framebuffer_Integer_Color_Sample_Counts: Sample_Count_Flags :=
            Sample_Count_No_Bit;
    end record;

    package Format_Vectors is new Ada.Containers.Vectors(Positive, Format);

    type Image_Format_List_Create_Info is
        new In_Structure(Image_Format_List_Create_Info_Type) with
    record
        View_Formats: Format_Vectors.Vector;
    end record;

    type Attachment_Description_2 is
        new In_Structure(Attachment_Description_2_Type) with
    record
        Flags: Attachment_Description_Flags := Attachment_Description_No_Bit;
        Format: Vulkan.Format;
        Samples: Sample_Count_Flags := Sample_Count_No_Bit;
        Load_Op: Attachment_Load_Op;
        Store_Op: Attachment_Store_Op;
        Stencil_Load_Op: Attachment_Load_Op;
        Stencil_Store_Op: Attachment_Store_Op;
        Initial_Layout: Image_Layout;
        Final_Layout: Image_Layout;
    end record;

    package Attachment_Description_2_Vectors is
        new Ada.Containers.Vectors(Positive, Attachment_Description_2);

    type Attachment_Reference_2 is
        new In_Structure(Attachment_Reference_2_Type) with
    record
        Attachment: Interfaces.Unsigned_32;
        Layout: Image_Layout;
        Aspect_Mask: Image_Aspect_Flags;
    end record;

    type Attachment_Reference_2_Access is access constant Attachment_Reference_2
        with Storage_Size => 0;

    package Attachment_Reference_2_Vectors is
        new Ada.Containers.Vectors(Positive, Attachment_Reference_2);

    type Subpass_Description_2 is
        new In_Structure(Subpass_Description_2_Type) with
    record
        Flags: Subpass_Description_Flags := Subpass_Description_No_Bit;
        Pipeline_Bind_Point: Vulkan.Pipeline_Bind_Point;
        View_Mask: Interfaces.Unsigned_32;
        Input_Attachments: Attachment_Reference_2_Vectors.Vector;
        Color_Attachments: Attachment_Reference_2_Vectors.Vector;
        Resolve_Attachments: Attachment_Reference_2_Vectors.Vector;
        Depth_Stencil_Attachment: Attachment_Reference_2_Access;
        Preserve_Attachments: Unsigned_32_Vectors.Vector;
    end record;

    package Subpass_Description_2_Vectors is
        new Ada.Containers.Vectors(Positive, Subpass_Description_2);

    type Subpass_Dependency_2 is
        new In_Structure(Subpass_Dependency_2_Type) with
    record
        Src_Subpass: Interfaces.Unsigned_32;
        Dst_Subpass: Interfaces.Unsigned_32;
        Src_Stage_Mask: Pipeline_Stage_Flags := Pipeline_Stage_No_Bit;
        Dst_Stage_Mask: Pipeline_Stage_Flags := Pipeline_Stage_No_Bit;
        Src_Access_Mask: Access_Flags := Access_No_Bit;
        Dst_Access_Mask: Access_Flags := Access_No_Bit;
        Dependency_Flags: Vulkan.Dependency_Flags := Dependency_No_Bit;
        View_Offset: Interfaces.Unsigned_32;
    end record;

    package Subpass_Dependency_2_Vectors is
        new Ada.Containers.Vectors(Positive, Subpass_Dependency_2);

    type Render_Pass_Create_Info_2 is
        new In_Structure(Render_Pass_Create_Info_2_Type) with
    record
        Flags: Render_Pass_Create_Flags := Render_Pass_Create_No_Bit;
        Attachments: Attachment_Description_2_Vectors.Vector;
        Subpasses: Subpass_Description_2_Vectors.Vector;
        Dependencies: Subpass_Dependency_2_Vectors.Vector;
        Correlated_View_Masks: Unsigned_32_Vectors.Vector;
    end record;

    type Subpass_Begin_Info is new In_Structure(Subpass_Begin_Info_Type) with
    record
        Contents: Subpass_Contents;
    end record;

    type Subpass_End_Info is new In_Structure(Subpass_End_Info_Type) with
        null record;

    type Physical_Device_8Bit_Storage_Features is
        new Out_Structure(Physical_Device_8Bit_Storage_Features_Type) with
    record
        Storage_Buffer_8Bit_Access: Boolean;
        Uniform_And_Storage_Buffer_8Bit_Access: Boolean;
        Storage_Push_Constant_8: Boolean;
    end record;

    type Physical_Device_Driver_Properties is
        new Out_Structure(Physical_Device_Driver_Properties_Type) with
    record
        Driver_ID: Vulkan.Driver_ID;
        Driver_Name: Ada.Strings.Unbounded.Unbounded_String;
        Driver_Info: Ada.Strings.Unbounded.Unbounded_String;
        Conformance_Version: Vulkan.Conformance_Version;
    end record;

    type Physical_Device_Shader_Atomic_Int64_Features is new Out_Structure
        (Physical_Device_Shader_Atomic_Int64_Features_Type) with
    record
        Shader_Buffer_Int64_Atomics: Boolean;
        Shader_Shared_Int64_Atomics: Boolean;
    end record;

    type Physical_Device_Shader_Float16_Int8_Features is new Out_Structure
        (Physical_Device_Shader_Float16_Int8_Features_Type) with
    record
        Shader_Float16: Boolean;
        Shader_Int8: Boolean;
    end record;

    type Physical_Device_Float_Controls_Properties is new Out_Structure
        (Physical_Device_Float_Controls_Properties_Type) with
    record
        Denorm_Behavior_Independence: Shader_Float_Controls_Independence;
        Rounding_Mode_Independence: Shader_Float_Controls_Independence;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float16: Boolean;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float32: Boolean;
        Shader_Signed_Zero_Inf_Nan_Preserve_Float64: Boolean;
        Shader_Denorm_Preserve_Float16: Boolean;
        Shader_Denorm_Preserve_Float32: Boolean;
        Shader_Denorm_Preserve_Float64: Boolean;
        Shader_Denorm_Flush_To_Zero_Float16: Boolean;
        Shader_Denorm_Flush_To_Zero_Float32: Boolean;
        Shader_Denorm_Flush_To_Zero_Float64: Boolean;
        Shader_Rounding_Mode_RTE_Float16: Boolean;
        Shader_Rounding_Mode_RTE_Float32: Boolean;
        Shader_Rounding_Mode_RTE_Float64: Boolean;
        Shader_Rounding_Mode_RTZ_Float16: Boolean;
        Shader_Rounding_Mode_RTZ_Float32: Boolean;
        Shader_Rounding_Mode_RTZ_Float64: Boolean;
    end record;

    package Descriptor_Binding_Flags_Vectors is
        new Ada.Containers.Vectors(Positive, Descriptor_Binding_Flags);

    type Descriptor_Set_Layout_Binding_Flags_Create_Info is new In_Structure
        (Descriptor_Set_Layout_Binding_Flags_Create_Info_Type) with
    record
        Binding_Flags: Descriptor_Binding_Flags_Vectors.Vector;
    end record;

    type Physical_Device_Descriptor_Indexing_Features is new Out_Structure
        (Physical_Device_Descriptor_Indexing_Features_Type) with
    record
        Shader_Input_Attachment_Array_Dynamic_Indexing: Boolean;
        Shader_Uniform_Texel_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Storage_Texel_Buffer_Array_Dynamic_Indexing: Boolean;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Image_Array_Non_Uniform_Indexing: Boolean;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing: Boolean;
        Shader_Uniform_Texel_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Shader_Storage_Texel_Buffer_Array_Non_Uniform_Indexing: Boolean;
        Descriptor_Binding_Uniform_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Sampled_Image_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Image_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Uniform_Texel_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Storage_Texel_Buffer_Update_After_Bind: Boolean;
        Descriptor_Binding_Update_Unused_While_Pending: Boolean;
        Descriptor_Binding_Partially_Bound: Boolean;
        Descriptor_Binding_Variable_Descriptor_Count: Boolean;
        Runtime_Descriptor_Array: Boolean;
    end record;

    type Physical_Device_Descriptor_Indexing_Properties is new Out_Structure
        (Physical_Device_Descriptor_Indexing_Properties_Type) with
    record
        Max_Update_After_Bind_Descriptors_In_All_Pools: Interfaces.Unsigned_32;
        Shader_Uniform_Buffer_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Sampled_Image_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Storage_Buffer_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Storage_Image_Array_Non_Uniform_Indexing_Native: Boolean;
        Shader_Input_Attachment_Array_Non_Uniform_Indexing_Native: Boolean;
        Robust_Buffer_Access_Update_After_Bind: Boolean;
        Quad_Divergent_Implicit_LOD: Boolean;
        Max_Per_Stage_Descriptor_Update_After_Bind_Samplers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
        Max_Per_Stage_Update_After_Bind_Resources: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Samplers: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Uniform_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Buffers_Dynamic:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Sampled_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Storage_Images:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Input_Attachments:
            Interfaces.Unsigned_32;
    end record;

    type Descriptor_Set_Variable_Descriptor_Count_Allocate_Info is
        new In_Structure
            (Descriptor_Set_Variable_Descriptor_Count_Allocate_Info_Type) with
    record
        Descriptor_Counts: Unsigned_32_Vectors.Vector;
    end record;

    type Descriptor_Set_Variable_Descriptor_Count_Layout_Support is
        new Out_Structure
            (Descriptor_Set_Variable_Descriptor_Count_Layout_Support_Type) with
    record
        Max_Variable_Descriptor_Count: Interfaces.Unsigned_32;
    end record;

    type Subpass_Description_Depth_Stencil_Resolve is new In_Structure
        (Subpass_Description_Depth_Stencil_Resolve_Type) with
    record
        Depth_Resolve_Mode: Resolve_Mode_Flags := Resolve_Mode_No_Bit;
        Stencil_Resolve_Mode: Resolve_Mode_Flags := Resolve_Mode_No_Bit;
        Depth_Stencil_Resolve_Attachment: Attachment_Reference_2_Access;
    end record;

    type Physical_Device_Depth_Stencil_Resolve_Properties is new Out_Structure
        (Physical_Device_Depth_Stencil_Resolve_Properties_Type) with
    record
        Supported_Depth_Resolve_Modes: Resolve_Mode_Flags :=
            Resolve_Mode_No_Bit;
        Supported_Stencil_Resolve_Modes: Resolve_Mode_Flags :=
            Resolve_Mode_No_Bit;
        Independent_Resolve_None: Boolean;
        Independent_Resolve: Boolean;
    end record;

    type Physical_Device_Scalar_Block_Layout_Features is new Out_Structure
        (Physical_Device_Scalar_Block_Layout_Features_Type) with
    record
        Scalar_Block_Layout: Boolean;
    end record;

    type Image_Stencil_Usage_Create_Info is new In_Structure
        (Image_Stencil_Usage_Create_Info_Type) with
    record
        Stencil_Usage: Image_Usage_Flags := Image_Usage_No_Bit;
    end record;

    type Sampler_Reduction_Mode_Create_Info is new In_Structure
        (Sampler_Reduction_Mode_Create_Info_Type) with
    record
        Reduction_Mode: Sampler_Reduction_Mode;
    end record;

    type Physical_Device_Sampler_Filter_Minmax_Properties is new Out_Structure
        (Physical_Device_Sampler_Filter_Minmax_Properties_Type) with
    record
        Filter_Minmax_Single_Component_Formats: Boolean;
        Filter_Minmax_Image_Component_Mapping: Boolean;
    end record;

    type Physical_Device_Vulkan_Memory_Model_Features is new Out_Structure
        (Physical_Device_Vulkan_Memory_Model_Features_Type) with
    record
        Vulkan_Memory_Model: Boolean;
        VUlkan_Memory_Model_Device_Scope: Boolean;
        Vulkan_Memory_Model_Availability_Visibility_Chains: Boolean;
    end record;

    type Physical_Device_Imageless_Framebuffer_Features is new Out_Structure
        (Physical_Device_Imageless_Framebuffer_Features_Type) with
    record
        Imageless_Framebuffer: Boolean;
    end record;

    type Framebuffer_Attachment_Image_Info is new In_Structure
        (Framebuffer_Attachment_Image_Info_Type) with
    record
        Flags: Image_Create_Flags := Image_Create_No_Bit;
        Usage: Image_Usage_Flags := Image_Usage_No_Bit;
        Width: Vulkan.Width;
        Height: Vulkan.Height;
        Layer_Count: Vulkan.Array_Layers;
        View_Formats: Format_Vectors.Vector;
    end record;

    package Framebuffer_Attachment_Image_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Framebuffer_Attachment_Image_Info);

    type Framebuffer_Attachments_Create_Info is new In_Structure
        (Framebuffer_Attachments_Create_Info_Type) with
    record
        Attachment_Image_Infos:
            Framebuffer_Attachment_Image_Info_Vectors.Vector;
    end record;

    type Render_Pass_Attachment_Begin_Info is new In_Structure
        (Render_Pass_Attachment_Begin_Info_Type) with
    record
        Attachments: Image_View_Vectors.Vector;
    end record;

    type Physical_Device_Uniform_Buffer_Standard_Layout_Features is
        new Out_Structure
            (Physical_Device_Uniform_Buffer_Standard_Layout_Features_Type) with
    record
        Uniform_Buffer_Standard_Layout: Boolean;
    end record;

    type Physical_Device_Shader_Subgroup_Extended_Types_Features is
        new Out_Structure
            (Physical_Device_Shader_Subgroup_Extended_Types_Features_Type) with
    record
        Shader_Subgroup_Extended_Types: Boolean;
    end record;

    type Physical_Device_Separate_Depth_Stencil_Layouts_Features is
        new Out_Structure
            (Physical_Device_Separate_Depth_Stencil_Layouts_Features_Type) with
    record
        Separate_Depth_Stencil_Layouts: Boolean;
    end record;

    type Attachment_Reference_Stencil_Layout is new Out_Structure
        (Attachment_Reference_Stencil_Layout_Type) with
    record
        Stencil_Layout: Image_Layout;
    end record;

    type Attachment_Description_Stencil_Layout is new Out_Structure
        (Attachment_Description_Stencil_Layout_Type) with
    record
        Stencil_Initial_Layout: Image_Layout;
        Stencil_Final_Layout: Image_Layout;
    end record;

    type Physical_Device_Host_Query_Reset_Features is new Out_Structure
        (Physical_Device_Host_Query_Reset_Features_Type) with
    record
        Host_Query_Reset: Boolean;
    end record;

    type Physical_Device_Timeline_Semaphore_Features is new Out_Structure
        (Physical_Device_Timeline_Semaphore_Features_Type) with
    record
        Timeline_Semaphore: Boolean;
    end record;

    type Physical_Device_Timeline_Semaphore_Properties is new Out_Structure
        (Physical_Device_Timeline_Semaphore_Properties_Type) with
    record
        Max_Timeline_Semaphore_Value_Difference: Semaphore_Value;
    end record;

    type Semaphore_Type_Create_Info is new In_Structure
        (Semaphore_Type_Create_Info_Type) with
    record
        Semaphore_Type: Vulkan.Semaphore_Type;
        Initial_Value: Semaphore_Value;
    end record;

    package Semaphore_Value_Vectors is new Ada.Containers.Vectors
        (Positive, Semaphore_Value);

    type Timeline_Semaphore_Submit_Info is new In_Structure
        (Timeline_Semaphore_Submit_Info_Type) with
    record
        Wait_Semaphore_Values: Semaphore_Value_Vectors.Vector;
        Signal_Semaphore_Values: Semaphore_Value_Vectors.Vector;
    end record;

    type Semaphore_Wait_Info is new In_Structure(Semaphore_Wait_Info_Type) with
    record
        Flags: Semaphore_Wait_Flags := Semaphore_Wait_No_Bit;
        Semaphores: Semaphore_Vectors.Vector;
        Values: Semaphore_Value_Vectors.Vector;
    end record;

    type Semaphore_Signal_Info is new In_Structure
        (Semaphore_Signal_Info_Type) with
    record
        Semaphore: Vulkan.Semaphore;
        Value: Semaphore_Value;
    end record;

    type Physical_Device_Buffer_Device_Address_Features is new Out_Structure
        (Physical_Device_Buffer_Device_Address_Features_Type) with
    record
        Buffer_Device_Address: Boolean;
        Buffer_Device_Address_Capture_Replay: Boolean;
        Buffer_Device_Address_Multi_Device: Boolean;
    end record;

    type Buffer_Device_Address_Info is new In_Structure
        (Buffer_Device_Address_Info_Type) with
    record
        Buffer: Vulkan.Buffer;
    end record;
    
    type Buffer_Opaque_Capture_Address_Create_Info is new In_Structure
        (Buffer_Opaque_Capture_Address_Create_Info_Type) with
    record
        Opaque_Capture_Address: Interfaces.Unsigned_64;
    end record;

    type Memory_Opaque_Capture_Address_Allocate_Info is new In_Structure
        (Memory_Opaque_Capture_Address_Allocate_Info_Type) with
    record
        Opaque_Capture_Address: Interfaces.Unsigned_64;
    end record;

    type Device_Memory_Opaque_Capture_Address_Info is new In_Structure
        (Device_Memory_Opaque_Capture_Address_Info_Type) with
    record
        Memory: Device_Memory;
    end record;

    -- Vulkan 1.3
    type Physical_Device_Vulkan_1_3_Features is new Out_Structure
        (Physical_Device_Vulkan_1_3_Features_Type) with
    record
        Robust_Image_Access: Boolean;
        Inline_Uniform_Block: Boolean;
        Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind: Boolean;
        Pipeline_Creation_Cache_Control: Boolean;
        Private_Data: Boolean;
        Shader_Demote_To_Helper_Invocation: Boolean;
        Shader_Terminate_Invocation: Boolean;
        Subgroup_Size_Control: Boolean;
        Compute_Full_Subgroups: Boolean;
        Synchronization_2: Boolean;
        Texture_Compression_ASTC_HDR: Boolean;
        Shader_Zero_Initialize_Workgroup_Memory: Boolean;
        Dynamic_Rendering: Boolean;
        Shader_Integer_Dot_Product: Boolean;
        Maintenance_4: Boolean;
    end record;

    type Physical_Device_Vulkan_1_3_Properties is new Out_Structure
        (Physical_Device_Vulkan_1_3_Properties_Type) with
    record
        Min_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Compute_Workgroup_Subgroups: Interfaces.Unsigned_32;
        Required_Subgroup_Size_Stages: Shader_Stage_Flags :=
            Shader_Stage_No_Bit;
        Max_Inline_Uniform_Block_Size: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Inline_Uniform_Total_Size: Interfaces.Unsigned_32;
        Integer_Dot_Product_8Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_8Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_16Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_16Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated:
            Boolean;
  Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated:
            Boolean;
   Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated:
            Boolean;
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated:
            Boolean;
        Storage_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Storage_Texel_Buffer_Offset_Single_Texel_Alignment: Boolean;
        Uniform_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Uniform_Texel_Buffer_Offset_Single_Texel_Alignment: Boolean;
        Max_Buffer_Size: Device_Size;
    end record;

    type Pipeline_Creation_Feedback is
    record
        Flags: Pipeline_Creation_Feedback_Flags :=
            Pipeline_Creation_Feedback_No_Bit;
        Duration: Interfaces.Unsigned_64;
    end record
        with Convention => C;

    type Pipeline_Creation_Feedback_Access is access Pipeline_Creation_Feedback
        with Storage_Size => 0,
             Convention => C;

    -- Can't use a vector here for the stage feedbacks because this
    -- is a rare case of an input structure with mutable data references.
    type Pipeline_Creation_Feedback_Create_Info is new In_Structure
        (Pipeline_Creation_Feedback_Create_Info_Type) with
    record
        Pipeline_Creation_Feedback: Pipeline_Creation_Feedback_Access;
        Pipeline_Stage_Creation_Feedback_Count: Interfaces.Unsigned_32;
        Pipeline_Stage_Creation_Feedbacks: Pipeline_Creation_Feedback_Access;
    end record;

    type Physical_Device_Shader_Terminate_Invocation_Features is
        new Out_Structure
            (Physical_Device_Shader_Terminate_Invocation_Features_Type) with
    record
        Shader_Terminate_Invocation: Boolean;
    end record;

    type Physical_Device_Tool_Properties is new Out_Structure
        (Physical_Device_Tool_Properties_Type) with
    record
        Name: Ada.Strings.Unbounded.Unbounded_String;
        Version: Ada.Strings.Unbounded.Unbounded_String;
        Purposes: Tool_Purpose_Flags := Tool_Purpose_No_Bit;
        Description: Ada.Strings.Unbounded.Unbounded_String;
        Layer: Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Physical_Device_Tool_Properties_Vectors is
        new Ada.Containers.Vectors(Positive, Physical_Device_Tool_Properties);

    type Physical_Device_Shader_Demote_To_Helper_Invocation_Features is
        new Out_Structure
            (Physical_Device_Shader_Demote_To_Helper_Invocation_Features_Type)
                with
    record
        Shader_Demote_To_Helper_Invocation: Boolean;
    end record;

    type Physical_Device_Private_Data_Features is new Out_Structure
        (Physical_Device_Private_Data_Features_Type) with
    record
        Private_Data: Boolean;
    end record;

    type Device_Private_Data_Create_Info is new In_Structure
        (Device_Private_Data_Create_Info_Type) with
    record
        Private_Data_Slot_Request_Count: Interfaces.Unsigned_32;
    end record;

    type Private_Data_Slot_Create_Info is new In_Structure
        (Private_Data_Slot_Create_Info_Type) with
    record
        Flags: Private_Data_Slot_Create_Flags :=
            Private_Data_Slot_Create_No_Bit;
    end record;

    type Physical_Device_Pipeline_Creation_Cache_Control_Features is
        new Out_Structure
            (Physical_Device_Pipeline_Creation_Cache_Control_Features_Type) with
    record
        Pipeline_Creation_Cache_Control: Boolean;
    end record;

    type Memory_Barrier_2 is new In_Structure(Memory_Barrier_2_Type) with
    record
        Src_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Src_Access_Mask: Access_Flags_2 := Access_2_None;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Dst_Access_Mask: Access_Flags_2 := Access_2_None;
    end record;

    package Memory_Barrier_2_Vectors is new Ada.Containers.Vectors
        (Positive, Memory_Barrier_2);

    type Buffer_Memory_Barrier_2 is new In_Structure
        (Buffer_Memory_Barrier_2_Type) with
    record
        Src_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Src_Access_Mask: Access_Flags_2 := Access_2_None;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Dst_Access_Mask: Access_Flags_2 := Access_2_None;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Size: Device_Size;
    end record;

    package Buffer_Memory_Barrier_2_Vectors is new Ada.Containers.Vectors
        (Positive, Buffer_Memory_Barrier_2);

    type Image_Memory_Barrier_2 is new In_Structure
        (Image_Memory_Barrier_2_Type) with
    record
        Src_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Src_Access_Mask: Access_Flags_2 := Access_2_None;
        Dst_Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Dst_Access_Mask: Access_Flags_2 := Access_2_None;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Src_Queue_Family_Index: Queue_Family_Index;
        Dst_Queue_Family_Index: Queue_Family_Index;
        Image: Vulkan.Image;
        Subresource_Range: Image_Subresource_Range;
    end record;

    package Image_Memory_Barrier_2_Vectors is new Ada.Containers.Vectors
        (Positive, Image_Memory_Barrier_2);

    type Dependency_Info is new In_Structure(Dependency_Info_Type) with
    record
        Dependency_Flags: Vulkan.Dependency_Flags := Dependency_No_Bit;
        Memory_Barriers: Memory_Barrier_2_Vectors.Vector;
        Buffer_Memory_Barriers: Buffer_Memory_Barrier_2_Vectors.Vector;
        Image_Memory_Barriers: Image_Memory_Barrier_2_Vectors.Vector;
    end record;

    package Dependency_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Dependency_Info);

    type Semaphore_Submit_Info is new In_Structure
        (Semaphore_Submit_Info_Type) with
    record
        Semaphore: Vulkan.Semaphore;
        Value: Semaphore_Value;
        Stage_Mask: Pipeline_Stage_Flags_2 := Pipeline_Stage_2_None;
        Device_Index: Interfaces.Unsigned_32;
    end record;

    package Semaphore_Submit_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Semaphore_Submit_Info);

    type Command_Buffer_Submit_Info is new In_Structure
        (Command_Buffer_Submit_Info_Type) with
    record
        Command_Buffer: Vulkan.Command_Buffer;
        Device_Mask: Interfaces.Unsigned_32;
    end record;

    package Command_Buffer_Submit_Info_Vectors is new Ada.Containers.Vectors
        (Positive, Command_Buffer_Submit_Info);

    type Submit_Info_2 is new In_Structure(Submit_Info_2_Type) with
    record
        Flags: Submit_Flags := Submit_No_Bit;
        Wait_Semaphore_Infos: Semaphore_Submit_Info_Vectors.Vector;
        Command_Buffer_Infos: Command_Buffer_Submit_Info_Vectors.Vector;
        Signal_Semaphore_Infos: Semaphore_Submit_Info_Vectors.Vector;
    end record;

    package Submit_Info_2_Vectors is new Ada.Containers.Vectors(Positive,
                                                                Submit_Info_2);

    type Physical_Device_Synchronization_2_Features is new Out_Structure
        (Physical_Device_Synchronization_2_Features_Type) with
    record
        Synchronization_2: Boolean;
    end record;

    type Physical_Device_Zero_Initialize_Workgroup_Memory_Features is
        new Out_Structure
            (Physical_Device_Zero_Initialize_Workgroup_Memory_Features_Type)
        with
    record
        Shader_Zero_Initialize_Workgroup_Memory: Boolean;
    end record;

    type Physical_Device_Image_Robustness_Features is new Out_Structure
        (Physical_Device_Image_Robustness_Features_Type) with
    record
        Robust_Image_Access: Boolean;
    end record;

    type Buffer_Copy_2 is new In_Structure(Buffer_Copy_2_Type) with
    record
        Src_Offset: Device_Size;
        Dst_Offset: Device_Size;
        Size: Device_Size;
    end record;

    package Buffer_Copy_2_Vectors is new Ada.Containers.Vectors(Positive,
                                                                Buffer_Copy_2);

    type Copy_Buffer_Info_2 is new In_Structure(Copy_Buffer_Info_2_Type) with
    record
        Src_Buffer: Buffer;
        Dst_Buffer: Buffer;
        Regions: Buffer_Copy_2_Vectors.Vector;
    end record;

    type Image_Copy_2 is new In_Structure(Image_Copy_2_Type) with
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record;

    package Image_Copy_2_Vectors is new Ada.Containers.Vectors(Positive,
                                                               Image_Copy_2);

    type Copy_Image_Info_2 is new In_Structure(Copy_Image_Info_2_Type) with
    record
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Image_Copy_2_Vectors.Vector;
    end record;

    type Buffer_Image_Copy_2 is new In_Structure(Buffer_Image_Copy_2_Type) with
    record
        Buffer_Offset: Device_Size;
        Buffer_Row_Length: Width;
        Buffer_Image_Height: Height;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record;

    package Buffer_Image_Copy_2_Vectors is new Ada.Containers.Vectors
        (Positive, Buffer_Image_Copy_2);

    type Copy_Buffer_To_Image_Info_2 is new In_Structure
        (Copy_Buffer_To_Image_Info_2_Type) with
    record
        Src_Buffer: Buffer;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Buffer_Image_Copy_2_Vectors.Vector;
    end record;

    type Copy_Image_To_Buffer_Info_2 is new In_Structure
        (Copy_Image_To_Buffer_Info_2_Type) with
    record
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Buffer: Buffer;
        Regions: Buffer_Image_Copy_2_Vectors.Vector;
    end record;

    type Image_Blit_2 is new In_Structure(Image_Blit_2_Type) with
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offsets: Offset_3D_Array(1 .. 2);
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offsets: Offset_3D_Array(1 .. 2);
    end record;

    package Image_Blit_2_Vectors is new Ada.Containers.Vectors(Positive,
                                                               Image_Blit_2);

    type Blit_Image_Info_2 is new In_Structure(Blit_Image_Info_2_Type) with
    record
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Image_Blit_2_Vectors.Vector;
        Filter: Vulkan.Filter;
    end record;

    type Image_Resolve_2 is new In_Structure(Image_Resolve_2_Type) with
    record
        Src_Subresource: Image_Subresource_Layers;
        Src_Offset: Offset_3D;
        Dst_Subresource: Image_Subresource_Layers;
        Dst_Offset: Offset_3D;
        Extent: Extent_3D;
    end record;

    package Image_Resolve_2_Vectors is new Ada.Containers.Vectors
        (Positive, Image_Resolve_2);

    type Resolve_Image_Info_2 is new In_Structure
        (Resolve_Image_Info_2_Type) with
    record
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Image_Resolve_2_Vectors.Vector;
    end record;

    type Physical_Device_Subgroup_Size_Control_Features is new Out_Structure
        (Physical_Device_Subgroup_Size_Control_Features_Type) with
    record
        Subgroup_Size_Control: Boolean;
        Compute_Full_Subgroups: Boolean;
    end record;

    type Physical_Device_Subgroup_Size_Control_Properties is new Out_Structure
        (Physical_Device_Subgroup_Size_Control_Properties_Type) with
    record
        Min_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Subgroup_Size: Interfaces.Unsigned_32;
        Max_Compute_Workgroup_Subgroups: Interfaces.Unsigned_32;
        Required_Subgroup_Size_Stages: Shader_Stage_Flags :=
            Shader_Stage_No_Bit;
    end record;

    type Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info is
        new Out_Structure
            (Pipeline_Shader_Stage_Required_Subgroup_Size_Create_Info_Type) with
    record
        Required_Subgroup_Size: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Inline_Uniform_Block_Features is new Out_Structure
        (Physical_Device_Inline_Uniform_Block_Features_Type) with
    record
        Inline_Uniform_Block: Boolean;
        Descriptor_Binding_Inline_Uniform_Block_Update_After_Bind: Boolean;
    end record;

    type Physical_Device_Inline_Uniform_Block_Properties is new Out_Structure
        (Physical_Device_Inline_Uniform_Block_Properties_Type) with
    record
        Max_Inline_Uniform_Block_Size: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Per_Stage_Descriptor_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
        Max_Descriptor_Set_Inline_Uniform_Blocks: Interfaces.Unsigned_32;
        Max_Descriptor_Set_Update_After_Bind_Inline_Uniform_Blocks:
            Interfaces.Unsigned_32;
    end record;

    type Write_Descriptor_Set_Inline_Uniform_Block is new In_Structure
        (Write_Descriptor_Set_Inline_Uniform_Block_Type) with
    record
        Data_Size: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record;

    type Descriptor_Pool_Inline_Uniform_Block_Create_Info is new In_Structure
        (Descriptor_Pool_Inline_Uniform_Block_Create_Info_Type) with
    record
        Max_Inline_Uniform_Block_Bindings: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Texture_Compression_ASTC_HDR_Features is
        new Out_Structure
            (Physical_Device_Texture_Compression_ASTC_HDR_Features_Type) with
    record
        Texture_Compression_ASTC_HDR: Boolean;
    end record;

    type Rendering_Attachment_Info(Clear_Type: Clear_Value_Type;
                                   Color_Type: Clear_Color_Type) is
         new In_Structure(Rendering_Attachment_Info_Type) with
    record
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
        Resolve_Mode: Resolve_Mode_Flags := Resolve_Mode_No_Bit;
        Resolve_Image_View: Vulkan.Image_View;
        Resolve_Image_Layout: Vulkan.Image_Layout;
        Load_Op: Attachment_Load_Op;
        Store_Op: Attachment_Store_Op;
        Clear_Value: Vulkan.Clear_Value(Clear_Color, Clear_Color_Float);
    end record;

    type Rendering_Attachment_Info_Access is
        access constant Rendering_Attachment_Info
        with Storage_Size => 0;

    package Rendering_Attachment_Info_Vectors is
        new Ada.Containers.Indefinite_Vectors(Positive,
                           Rendering_Attachment_Info);

    type Rendering_Info is new In_Structure(Rendering_Info_Type) with
    record
        Flags: Rendering_Flags := Rendering_No_Bit;
        Render_Area: Rect_2D;
        Layer_Count: Array_Layers;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachments: Rendering_Attachment_Info_Vectors.Vector;
        Depth_Attachment: Rendering_Attachment_Info_Access;
        Stencil_Attachment: Rendering_Attachment_Info_Access;
    end record;

    type Pipeline_Rendering_Create_Info is new In_Structure
        (Pipeline_Rendering_Create_Info_Type) with
    record
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Formats: Format_Vectors.Vector;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record;

    type Physical_Device_Dynamic_Rendering_Features is new Out_Structure
        (Physical_Device_Dynamic_Rendering_Features_Type) with
    record
        Dynamic_Rendering: Boolean;
    end record;

    type Command_Buffer_Inheritance_Rendering_Info is new In_Structure
        (Command_Buffer_Inheritance_Rendering_Info_Type) with
    record
        Flags: Rendering_Flags := Rendering_No_Bit;
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Formats: Format_Vectors.Vector;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
        Rasterization_Samples: Sample_Count_Flags := Sample_Count_No_Bit;
    end record;

    type Physical_Device_Shader_Integer_Dot_Product_Features is
        new Out_Structure
            (Physical_Device_Shader_Integer_Dot_Product_Features_Type) with
    record
        Shader_Integer_Dot_Product: Boolean;
    end record;

    type Physical_Device_Shader_Integer_Dot_Product_Properties is
        new Out_Structure
            (Physical_Device_Shader_Integer_Dot_Product_Properties_Type) with
    record
        Integer_Dot_Product_8Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_8Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_8Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Signed_Accelerated: Boolean;
        Integer_Dot_Product_4x8Bit_Packed_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_16Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_16Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_16Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_32Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Unsigned_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Signed_Accelerated: Boolean;
        Integer_Dot_Product_64Bit_Mixed_Signedness_Accelerated: Boolean;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_8Bit_Signed_Accelerated:
            Boolean;
  Integer_Dot_Product_Accumulating_Saturating_8Bit_Mixed_Signedness_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Unsigned_Accelerated:
            Boolean;
   Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Signed_Accelerated:
            Boolean;
Integer_Dot_Product_Accumulating_Saturating_4x8Bit_Packed_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_16Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_16Bit_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_32Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_32Bit_Mixed_Signedness_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Unsigned_Accelerated:
            Boolean;
        Integer_Dot_Product_Accumulating_Saturating_64Bit_Signed_Accelerated:
            Boolean;
 Integer_Dot_Product_Accumulating_Saturating_64Bit_Mixed_Signedness_Accelerated:
            Boolean;
    end record;

    type Physical_Device_Texel_Buffer_Alignment_Properties is
        new Out_Structure
            (Physical_Device_Texel_Buffer_Alignment_Properties_Type) with
    record
        Storage_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Storage_Texel_Buffer_Offset_Single_Texel_Alignment: Boolean;
        Uniform_Texel_Buffer_Offset_Alignment_Bytes: Device_Size;
        Uniform_Texel_Buffer_Offset_Single_Texel_Alignment: Boolean;
    end record;

    type Format_Properties_3 is new Out_Structure(Format_Properties_3_Type) with
    record
        Linear_Tiling_Features: Format_Feature_Flags_2 :=
            Format_Feature_2_No_Bit;
        Optimal_Tiling_Features: Format_Feature_Flags_2 :=
            Format_Feature_2_No_Bit;
        Buffer_Features: Format_Feature_Flags_2 := Format_Feature_2_No_Bit;
    end record;

    type Physical_Device_Maintenance_4_Features is new Out_Structure
        (Physical_Device_Maintenance_4_Features_Type) with
    record
        Maintenance_4: Boolean;
    end record;

    type Physical_Device_Maintenance_4_Properties is new Out_Structure
        (Physical_Device_Maintenance_4_Properties_Type) with
    record
        Max_Buffer_Size: Device_Size;
    end record;

    type Device_Buffer_Memory_Requirements is new In_Structure
        (Device_Buffer_Memory_Requirements_Type) with
    record
        Create_Info: Buffer_Create_Info_Access;
    end record;

    type Device_Image_Memory_Requirements is new In_Structure
        (Device_Image_Memory_Requirements_Type) with
    record
        Create_Info: Image_Create_Info_Access;
        Plane_Aspect: Image_Aspect_Flags := Image_Aspect_No_Bit;
    end record;

    -- Vulkan 1.4
    type Physical_Device_Vulkan_1_4_Features is new Out_Structure
        (Physical_Device_Vulkan_1_4_Features_Type) with
    record
        Global_Priority_Query: Boolean;
        Shader_Subgroup_Rotate: Boolean;
        Shader_Subgroup_Rotate_Clustered: Boolean;
        Shader_Float_Controls_2: Boolean;
        Shader_Expect_Assume: Boolean;
        Rectangular_Lines: Boolean;
        Bresenham_Lines: Boolean;
        Smooth_Lines: Boolean;
        Stippled_Rectangular_Lines: Boolean;
        Stippled_Bresenham_Lines: Boolean;
        Stippled_Smooth_Lines: Boolean;
        Vertex_Attribute_Instance_Rate_Divisor: Boolean;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Boolean;
        Index_Type_Uint8: Boolean;
        Dynamic_Rendering_Local_Read: Boolean;
        Maintenance_5: Boolean;
        Maintenance_6: Boolean;
        Pipeline_Protected_Access: Boolean;
        Pipeline_Robustness: Boolean;
        Host_Image_Copy: Boolean;
        Push_Descriptor: Boolean;
    end record;

    package Image_Layout_Vectors is new Ada.Containers.Vectors(Positive,
                                                               Image_Layout);

    type Physical_Device_Vulkan_1_4_Properties is new Out_Structure
        (Physical_Device_Vulkan_1_4_Properties_Type) with
    record
        Line_Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Boolean;
        Max_Push_Descriptors: Interfaces.Unsigned_32;
        Dynamic_Rendering_Local_Read_Depth_Stencil_Attachments: Boolean;
        Dynamic_Rendering_Local_Read_Multisampled_Attachments: Boolean;
        Early_Fragment_Multisample_Coverage_After_Sample_Counting: Boolean;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting: Boolean;
        Depth_Stencil_Swizzle_One_Support: Boolean;
        Polygon_Mode_Point_Size: Boolean;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram: Boolean;
        Non_Strict_Wide_Lines_Use_Parallelogram: Boolean;
        Block_Texel_View_Compatible_Multiple_Layers: Boolean;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Boolean;
        Default_Robustness_Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images: Pipeline_Robustness_Image_Behavior;
        Copy_Src_Layouts: Image_Layout_Vectors.Vector;
        Copy_Dst_Layouts: Image_Layout_Vectors.Vector;
        Optimal_Tiling_Layout_UUID: UUID;
        Identical_Memory_Type_Requirements: Boolean;
    end record;

    type Device_Queue_Global_Priority_Create_Info is new In_Structure
        (Device_Queue_Global_Priority_Create_Info_Type) with
    record
        Global_Priority: Queue_Global_Priority;
    end record;

    type Physical_Device_Global_Priority_Query_Features is new Out_Structure
        (Physical_Device_Global_Priority_Query_Features_Type) with
    record
        Global_Priority_Query: Boolean;
    end record;

    type Queue_Global_Priority_Array is
        array (1 .. Max_Global_Priority_Size) of Queue_Global_Priority
        with Convention => C;

    type Queue_Family_Global_Priority_Properties is new Out_Structure
        (Queue_Family_Global_Priority_Properties_Type) with
    record
        Priority_Count: Interfaces.Unsigned_32;
        Priorities: Queue_Global_Priority_Array;
    end record;

    type Physical_Device_Shader_Subgroup_Rotate_Features is new Out_Structure
        (Physical_Device_Shader_Subgroup_Rotate_Features_Type) with
    record
        Shader_Subgroup_Rotate: Boolean;
        Shader_Subgroup_Rotate_Clustered: Boolean;
    end record;

    type Physical_Device_Shader_Float_Controls_2_Features is new Out_Structure
        (Physical_Device_Shader_Float_Controls_2_Features_Type) with
    record
        Shader_Float_Controls_2: Boolean;
    end record;

    type Physical_Device_Shader_Expect_Assume_Features is new Out_Structure
        (Physical_Device_Shader_Expect_Assume_Features_Type) with
    record
        Shader_Expect_Assume: Boolean;
    end record;

    type Physical_Device_Line_Rasterization_Features is new Out_Structure
        (Physical_Device_Line_Rasterization_Features_Type) with
    record
        Rectangular_Lines: Boolean;
        Bresenham_Lines: Boolean;
        Smooth_Lines: Boolean;
        Stippled_Rectangular_Lines: Boolean;
        Stippled_Bresenham_Lines: Boolean;
        Stippled_Smooth_Lines: Boolean;
    end record;

    type Physical_Device_Line_Rasterization_Properties is new Out_Structure
        (Physical_Device_Line_Rasterization_Properties_Type) with
    record
        Line_Sub_Pixel_Precision_Bits: Interfaces.Unsigned_32;
    end record;

    type Pipeline_Rasterization_Line_State_Create_Info is new In_Structure
        (Pipeline_Rasterization_Line_State_Create_Info_Type) with
    record
        Line_Rasterization_Mode: Vulkan.Line_Rasterization_Mode;
        Stippled_Line_Enable: Boolean;
        Line_Stipple_Factor: Interfaces.Unsigned_32;
        Line_Stipple_Pattern: Interfaces.Unsigned_16;
    end record;

    type Physical_Device_Vertex_Attribute_Divisor_Properties is
        new Out_Structure
            (Physical_Device_Vertex_Attribute_Divisor_Properties_Type) with
    record
        Max_Vertex_Attrib_Divisor: Interfaces.Unsigned_32;
        Supports_Non_Zero_First_Instance: Boolean;
    end record;

    type Vertex_Input_Binding_Divisor_Description is
    record
        Binding: Interfaces.Unsigned_32;
        Divisor: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    package Vertex_Input_Binding_Divisor_Description_Vectors is
        new Ada.Containers.Vectors(Positive,
                                   Vertex_Input_Binding_Divisor_Description);

    type Pipeline_Vertex_Input_Divisor_State_Create_Info is new In_Structure
        (Pipeline_Vertex_Input_Divisor_State_Create_Info_Type) with
    record
        Vertex_Binding_Divisors:
            Vertex_Input_Binding_Divisor_Description_Vectors.Vector;
    end record;

    type Physical_Device_Vertex_Attribute_Divisor_Features is new Out_Structure
        (Physical_Device_Vertex_Attribute_Divisor_Features_Type) with
    record
        Vertex_Attribute_Instance_Rate_Divisor: Boolean;
        Vertex_Attribute_Instance_Rate_Zero_Divisor: Boolean;
    end record;

    type Physical_Device_Index_Type_Uint8_Features is new Out_Structure
        (Physical_Device_Index_Type_Uint8_Features_Type) with
    record
        Index_Type_Uint8: Boolean;
    end record;

    type Memory_Map_Info is new In_Structure(Memory_Map_Info_Type) with
    record
        Flags: Memory_Map_Flags := Memory_Map_No_Bit;
        Memory: Device_Memory;
        Offset: Device_Size;
        Size: Device_Size;
    end record;

    type Memory_Unmap_Info is new In_Structure(Memory_Unmap_Info_Type) with
    record
        Flags: Memory_Unmap_Flags := Memory_Unmap_No_Bit;
        Memory: Device_Memory;
    end record;

    type Physical_Device_Maintenance_5_Features is new Out_Structure
        (Physical_Device_Maintenance_5_Features_Type) with
    record
        Maintenance_5: Boolean;
    end record;

    type Physical_Device_Maintenance_5_Properties is new Out_Structure
        (Physical_Device_Maintenance_5_Properties_Type) with
    record
        Early_Fragment_Multisample_Coverage_After_Sample_Counting: Boolean;
        Early_Fragment_Sample_Mask_Test_Before_Sample_Counting: Boolean;
        Depth_Stencil_Swizzle_One_Support: Boolean;
        Polygon_Mode_Point_Size: Boolean;
        Non_Strict_Single_Pixel_Wide_Lines_Use_Parallelogram: Boolean;
        Non_Strict_Wide_Lines_Use_Parallelogram: Boolean;
    end record;

    type Rendering_Area_Info is new In_Structure(Rendering_Area_Info_Type) with
    record
        View_Mask: Interfaces.Unsigned_32;
        Color_Attachment_Formats: Format_Vectors.Vector;
        Depth_Attachment_Format: Format;
        Stencil_Attachment_Format: Format;
    end record;

    type Image_Subresource_2 is new Out_Structure(Image_Subresource_2_Type) with
    record
        Image_Subresource: Vulkan.Image_Subresource;
    end record;

    type Image_Subresource_2_Access is access constant Image_Subresource_2
        with Storage_Size => 0;

    type Device_Image_Subresource_Info is new In_Structure
        (Device_Image_Subresource_Info_Type) with
    record
        Create_Info: Image_Create_Info_Access;
        Subresource: Image_Subresource_2_Access;
    end record;

    type Subresource_Layout_2 is new Out_Structure
        (Subresource_Layout_2_Type) with
    record
        Subresource_Layout: Vulkan.Subresource_Layout;
    end record;

    type Pipeline_Create_Flags_2_Create_Info is new In_Structure
        (Pipeline_Create_Flags_2_Create_Info_Type) with
    record
        Flags: Pipeline_Create_Flags_2 := Pipeline_Create_2_No_Bit;
    end record;

    type Buffer_Usage_Flags_2_Create_Info is new In_Structure
        (Buffer_Usage_Flags_2_Create_Info_Type) with
    record
        Flags: Buffer_Usage_Flags_2 := Buffer_Usage_2_No_Bit;
    end record;

    type Physical_Device_Push_Descriptor_Properties is new Out_Structure
        (Physical_Device_Push_Descriptor_Properties_Type) with
    record
        Max_Push_Descriptors: Interfaces.Unsigned_32;
    end record;

    type Physical_Device_Dynamic_Rendering_Local_Read_Features is
        new Out_Structure
            (Physical_Device_Dynamic_Rendering_Local_Read_Features_Type) with
    record
        Dynamic_Rendering_Local_Read: Boolean;
    end record;

    type Rendering_Attachment_Location_Info is new In_Structure
        (Rendering_Attachment_Location_Info_Type) with
    record
        Color_Attachment_Locations: Unsigned_32_Vectors.Vector;
    end record;

    type Unsigned_32_Access is access constant Interfaces.Unsigned_32
        with Convention => C,
             Storage_Size => 0;

    type Rendering_Input_Attachment_Index_Info is new In_Structure
        (Rendering_Input_Attachment_Index_Info_Type) with
    record
        Color_Attachment_Input_Indices: Unsigned_32_Vectors.Vector;
        Depth_Input_Attachment_Index: Unsigned_32_Access;
        Stencil_Input_Attachment_Index: Unsigned_32_Access;
    end record;

    type Physical_Device_Maintenance_6_Features is new Out_Structure
        (Physical_Device_Maintenance_6_Features_Type) with
    record
        Maintenance_6: Boolean;
    end record;

    type Physical_Device_Maintenance_6_Properties is new Out_Structure
        (Physical_Device_Maintenance_6_Features_Type) with
    record
        Block_Texel_View_Compatible_Multiple_Layers: Boolean;
        Max_Combined_Image_Sampler_Descriptor_Count: Interfaces.Unsigned_32;
        Fragment_Shading_Rate_Clamp_Combiner_Inputs: Boolean;
    end record;

    type Result_Access is access all Result
        with Convention => C,
             Storage_Size => 0;

    type Bind_Memory_Status is new In_Structure(Bind_Memory_Status_Type) with
    record
        Result: Result_Access;
    end record;

    type Bind_Descriptor_Sets_Info is new In_Structure
        (Bind_Descriptor_Sets_Info_Type) with
    record
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Layout: Pipeline_Layout;
        First_Set: Interfaces.Unsigned_32;
        Descriptor_Sets: Descriptor_Set_Vectors.Vector;
        Dynamic_Offsets: Unsigned_32_Vectors.Vector;
    end record;

    type Push_Constants_Info is new In_Structure(Push_Constants_Info_Type) with
    record
        Layout: Pipeline_Layout;
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Offset: Interfaces.Unsigned_32;
        Size: Interfaces.Unsigned_32;
        Values: Interfaces.C.Extensions.void_ptr;
    end record;

    type Push_Descriptor_Set_Info is new In_Structure
        (Push_Descriptor_Set_Info_Type) with
    record
        Stage_Flags: Shader_Stage_Flags := Shader_Stage_No_Bit;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Descriptor_Writes: Write_Descriptor_Set_Vectors.Vector;
    end record;

    type Push_Descriptor_Set_With_Template_Info is new In_Structure
        (Push_Descriptor_Set_With_Template_Info_Type) with
    record
        Descriptor_Update_Template: Vulkan.Descriptor_Update_Template;
        Layout: Pipeline_Layout;
        Set: Interfaces.Unsigned_32;
        Data: Interfaces.C.Extensions.void_ptr;
    end record;

    type Physical_Device_Pipeline_Protected_Access_Features is new Out_Structure
        (Physical_Device_Pipeline_Protected_Access_Features_Type) with
    record
        Pipeline_Protected_Access: Boolean;
    end record;

    type Physical_Device_Pipeline_Robustness_Features is new Out_Structure
        (Physical_Device_Pipeline_Robustness_Features_Type) with
    record
        Pipeline_Robustness: Boolean;
    end record;

    type Physical_Device_Pipeline_Robustness_Properties is new Out_Structure
        (Physical_Device_Pipeline_Robustness_Properties_Type) with
    record
        Default_Robustness_Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images: Pipeline_Robustness_Image_Behavior;
    end record;

    type Pipeline_Robustness_Create_Info is new In_Structure
        (Pipeline_Robustness_Create_Info_Type) with
    record
        Storage_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Uniform_Buffers: Pipeline_Robustness_Buffer_Behavior;
        Vertex_Inputs: Pipeline_Robustness_Buffer_Behavior;
        Images: Pipeline_Robustness_Image_Behavior;
    end record;

    type Physical_Device_Host_Image_Copy_Features is new Out_Structure
        (Physical_Device_Host_Image_Copy_Features_Type) with
    record
        Host_Image_Copy: Boolean;
    end record;

    type Image_Layout_Access is access all Image_Layout
        with Convention => C,
             Storage_Size => 0;

    -- Unfortunately, due to the way this structure works, you'll
    -- have to provid the pointers directly rather than just having
    -- a vector filled.
    type Physical_Device_Host_Image_Copy_Properties is new Out_Structure
        (Physical_Device_Host_Image_Copy_Properties_Type) with
    record
        Copy_Src_Layout_Count: Interfaces.Unsigned_32;
        Copy_Src_Layouts: Image_Layout_Access;
        Copy_Dst_Layout_Count: Interfaces.Unsigned_32;
        Copy_Dst_Layouts: Image_Layout_Access;
        Optimal_Tiling_Layout_UUID: UUID;
        Identical_Memory_Type_Requirements: Boolean;
    end record;

    type Memory_To_Image_Copy is new In_Structure
        (Memory_To_Image_Copy_Type) with
    record
        Host_Pointer: Interfaces.C.Extensions.void_ptr;
        Memory_Row_Length: Interfaces.Unsigned_32;
        Memory_Image_Height: Interfaces.Unsigned_32;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record;

    package Memory_To_Image_Copy_Vectors is
        new Ada.Containers.Vectors(Positive, Memory_To_Image_Copy);

    type Image_To_Memory_Copy is new In_Structure
        (Image_To_Memory_Copy_Type) with
    record
        Host_Pointer: Interfaces.C.Extensions.void_ptr;
        Memory_Row_Length: Interfaces.Unsigned_32;
        Memory_Image_Height: Interfaces.Unsigned_32;
        Image_Subresource: Image_Subresource_Layers;
        Image_Offset: Offset_3D;
        Image_Extent: Extent_3D;
    end record;

    package Image_To_Memory_Copy_Vectors is
        new Ada.Containers.Vectors(Positive, Image_To_Memory_Copy);

    type Copy_Memory_To_Image_Info is new In_Structure
        (Copy_Memory_To_Image_Info_Type) with
    record
        Flags: Host_Image_Copy_Flags := Host_Image_Copy_No_Bit;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Memory_To_Image_Copy_Vectors.Vector;
    end record;

    type Copy_Image_To_Memory_Info is new In_Structure
        (Copy_Image_To_Memory_Info_Type) with
    record
        Flags: Host_Image_Copy_Flags := Host_Image_Copy_No_Bit;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Regions: Image_To_Memory_Copy_Vectors.Vector;
    end record;

    type Copy_Image_To_Image_Info is
        new In_Structure(Copy_Image_To_Image_Info_Type) with
    record
        Flags: Host_Image_Copy_Flags := Host_Image_Copy_No_Bit;
        Src_Image: Image;
        Src_Image_Layout: Image_Layout;
        Dst_Image: Image;
        Dst_Image_Layout: Image_Layout;
        Regions: Image_Copy_2_Vectors.Vector;
    end record;

    type Host_Image_Layout_Transition_Info is new In_Structure
        (Host_Image_Layout_Transition_Info_Type) with
    record
        Image: Vulkan.Image;
        Old_Layout: Image_Layout;
        New_Layout: Image_Layout;
        Subresource_Range: Image_Subresource_Range;
    end record;

    package Host_Image_Layout_Transition_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Host_Image_Layout_Transition_Info);

    type Subresource_Host_Memcpy_Size is new Out_Structure
        (Subresource_Host_Memcpy_Size_Type) with
    record
        Size: Device_Size;
    end record;

    type Host_Image_Copy_Device_Performance_Query is new Out_Structure
        (Host_Image_Copy_Device_Performance_Query_Type) with
    record
        Optimal_Device_Access: Boolean;
        Identical_Memory_Layout: Boolean;
    end record;
end Vulkan;

