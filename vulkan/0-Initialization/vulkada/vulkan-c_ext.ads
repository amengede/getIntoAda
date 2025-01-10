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

-- Copyright 2024 Phaser Cat Games LLC

-- C interface for EXT records

with Interfaces.C.Strings;
with Vulkan.C;
with Vulkan.C_Arrays;
with Vulkan.Extensions.EXT;
with Vulkan.Extensions.KHR;
with Vulkan.Metal;

private package Vulkan.C_EXT is
    -- Extension record identifiers.
    subtype Structure is Structure_Type
        with Static_Predicate => Structure in
            Rendering_Fragment_Density_Map_Attachment_Info_Type |
            Physical_Device_Transform_Feedback_Features_Type |
            Physical_Device_Transform_Feedback_Properties_Type |
            Pipeline_Rasterization_State_Stream_Create_Info_Type |
            Image_View_ASTC_Decode_Mode_Type |
            Physical_Device_ASTC_Decode_Features_Type |
            Physical_Device_Pipeline_Robustness_Features_Type |
            Physical_Device_Pipeline_Robustness_Properties_Type |
            Pipeline_Robustness_Create_Info_Type |
            Conditional_Rendering_Begin_Info_Type |
            Physical_Device_Conditional_Rendering_Features_Type |
            Command_Buffer_Inheritance_Conditional_Rendering_Info_Type |
            Surface_Capabilities_2_EXT_Type |
            Display_Power_Info_Type |
            Device_Event_Info_Type |
            Display_Event_Info_Type |
            Swapchain_Counter_Create_Info_Type |
            Physical_Device_Discard_Rectangle_Properties_Type |
            Pipeline_Discard_Rectangle_State_Create_Info_Type |
            Physical_Device_Conservative_Rasterization_Properties_Type |
            Pipeline_Rasterization_Conservative_State_Create_Info_Type |
            Physical_Device_Depth_Clip_Enable_Features_Type |
            Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type |
            HDR_Metadata_Type |
            Debug_Utils_Label_Type |
            Debug_Utils_Object_Name_Info_Type |
            Debug_Utils_Messenger_Callback_Data_Type |
            Debug_Utils_Messenger_Create_Info_Type |
            Debug_Utils_Object_Tag_Info_Type |
            Metal_Surface_Create_Info_Type;

    subtype Out_Structure is Structure
        with Static_Predicate => Out_Structure in Out_Structure_Type;

    subtype In_Structure is Structure
        with Static_Predicate => In_Structure not in Out_Structure;

    -- C interface records.
    type Rendering_Fragment_Density_Map_Attachment_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Rendering_Fragment_Density_Map_Attachment_Info_Type;
        Next: C.In_Structure_C_Access;
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
    end record
        with Convention => C;

    type Rendering_Fragment_Density_Map_Attachment_Info_C_Access is
        access Rendering_Fragment_Density_Map_Attachment_Info_C
        with Convention => C;

    type Physical_Device_Transform_Feedback_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Transform_Feedback_Features_Type;
        Next: C.Out_Structure_C_Access;
        Transform_Feedback: Interfaces.Unsigned_32;
        Geometry_Streams: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Transform_Feedback_Features_C_Access is
        access Physical_Device_Transform_Feedback_Features_C
        with Convention => C;

    type Physical_Device_Transform_Feedback_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Transform_Feedback_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Transform_Feedback_Streams: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffers: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Size: Device_Size;
        Max_Transform_Feedback_Stream_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Stride: Interfaces.Unsigned_32;
        Transform_Feedback_Queries: Interfaces.Unsigned_32;
        Transform_Feedback_Streams_Lines_Triangles: Interfaces.Unsigned_32;
        Transform_Feedback_Rasterization_Stream_Select: Interfaces.Unsigned_32;
        Transform_Feedback_Draw: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Transform_Feedback_Properties_C_Access is
        access Physical_Device_Transform_Feedback_Properties_C
        with Convention => C;

    type Pipeline_Rasterization_State_Stream_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_State_Stream_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.EXT.Pipeline_Rasterization_State_Stream_Create_Flags;
        Rasterization_Stream: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Rasterization_State_Stream_Create_Info_C_Access is
        access Pipeline_Rasterization_State_Stream_Create_Info_C
        with Convention => C;

    type Image_View_ASTC_Decode_Mode_C is
    record
        Record_Type: In_Structure_Type := Image_View_ASTC_Decode_Mode_Type;
        Next: C.In_Structure_C_Access;
        Decode_Mode: Format;
    end record
        with Convention => C;

    type Image_View_ASTC_Decode_Mode_C_Access is
        access Image_View_ASTC_Decode_Mode_C
        with Convention => C;

    type Physical_Device_ASTC_Decode_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_ASTC_Decode_Features_Type;
        Next: C.Out_Structure_C_Access;
        Decode_Mode_Shared_Exponent: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_ASTC_Decode_Features_C_Access is
        access Physical_Device_ASTC_Decode_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Features_Type;
        Next: C.Out_Structure_C_Access;
        Pipeline_Robustness: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Features_C_Access is
        access Physical_Device_Pipeline_Robustness_Features_C
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Pipeline_Robustness_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Default_Robustness_Storage_Buffers:
            Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Uniform_Buffers:
            Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Vertex_Inputs:
            Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Default_Robustness_Images:
            Extensions.EXT.Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Physical_Device_Pipeline_Robustness_Properties_C_Access is
        access Physical_Device_Pipeline_Robustness_Properties_C
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Pipeline_Robustness_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Storage_Buffers: Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Uniform_Buffers: Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Vertex_Inputs: Extensions.EXT.Pipeline_Robustness_Buffer_Behavior;
        Images: Extensions.EXT.Pipeline_Robustness_Image_Behavior;
    end record
        with Convention => C;

    type Pipeline_Robustness_Create_Info_C_Access is
        access Pipeline_Robustness_Create_Info_C
        with Convention => C;

    type Conditional_Rendering_Begin_Info_C is
    record
        Record_Type: In_Structure_Type := Conditional_Rendering_Begin_Info_Type;
        Next: C.In_Structure_C_Access;
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Flags: Extensions.EXT.Conditional_Rendering_Flags;
    end record
        with Convention => C;

    type Conditional_Rendering_Begin_Info_C_Access is
        access Conditional_Rendering_Begin_Info_C
        with Convention => C;

    type Physical_Device_Conditional_Rendering_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Conditional_Rendering_Features_Type;
        Next: C.Out_Structure_C_Access;
        Conditional_Rendering: Interfaces.Unsigned_32;
        Inherited_Conditional_Rendering: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Conditional_Rendering_Features_C_Access is
        access Physical_Device_Conditional_Rendering_Features_C
        with Convention => C;

    type Command_Buffer_Inheritance_Conditional_Rendering_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Command_Buffer_Inheritance_Conditional_Rendering_Info_Type;
        Next: C.In_Structure_C_Access;
        Conditional_Rendering_Enable: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Command_Buffer_Inheritance_Conditional_Rendering_Info_C_Access is
        access Command_Buffer_Inheritance_Conditional_Rendering_Info_C
        with Convention => C;

    type Surface_Capabilities_2_C is
    record
        Record_Type: Out_Structure_Type := Surface_Capabilities_2_EXT_Type;
        Next: C.Out_Structure_C_Access;
        Min_Image_Count: Interfaces.Unsigned_32;
        Max_Image_Count: Interfaces.Unsigned_32;
        Current_Extent: Extent_2D;
        Min_Image_Extent: Extent_2D;
        Max_Image_Extent: Extent_2D;
        Max_Image_Array_Layers: Interfaces.Unsigned_32;
        Supported_Transforms: Extensions.KHR.Surface_Transform_Flags;
        Current_Transform: Extensions.KHR.Surface_Transform_Flags;
        Supported_Composite_Alpha: Extensions.KHR.Composite_Alpha_Flags;
        Supported_Usage_Flags: Image_Usage_Flags;
        Supported_Surface_Counters: Extensions.EXT.Surface_Counter_Flags;
    end record
        with Convention => C;

    type Surface_Capabilities_2_C_Access is
        access Surface_Capabilities_2_C
        with Convention => C;

    type Display_Power_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Power_Info_Type;
        Next: C.In_Structure_C_Access;
        Power_State: Extensions.EXT.Display_Power_State;
    end record
        with Convention => C;

    type Display_Power_Info_C_Access is access Display_Power_Info_C
        with Convention => C;

    type Device_Event_Info_C is
    record
        Record_Type: In_Structure_Type := Device_Event_Info_Type;
        Next: C.In_Structure_C_Access;
        Device_Event: Extensions.EXT.Device_Event_Type;
    end record
        with Convention => C;

    type Device_Event_Info_C_Access is access Device_Event_Info_C
        with Convention => C;

    type Display_Event_Info_C is
    record
        Record_Type: In_Structure_Type := Display_Event_Info_Type;
        Next: C.In_Structure_C_Access;
        Display_Event: Extensions.EXT.Display_Event_Type;
    end record
        with Convention => C;

    type Display_Event_Info_C_Access is access Display_Event_Info_C
        with Convention => C;

    type Swapchain_Counter_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Swapchain_Counter_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Surface_Counters: Extensions.EXT.Surface_Counter_Flags;
    end record
        with Convention => C;

    type Swapchain_Counter_Create_Info_C_Access is
        access Swapchain_Counter_Create_Info_C
        with Convention => C;

    type Physical_Device_Discard_Rectangle_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Discard_Rectangle_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Max_Discard_Rectangles: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Discard_Rectangle_Properties_C_Access is
        access Physical_Device_Discard_Rectangle_Properties_C
        with Convention => C;

    type Pipeline_Discard_Rectangle_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Discard_Rectangle_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.EXT.Pipeline_Discard_Rectangle_State_Create_Flags;
        Discard_Rectangle_Mode: Extensions.EXT.Discard_Rectangle_Mode;
        Discard_Rectangle_Count: Interfaces.Unsigned_32;
        Discard_Rectangles: C.Rect_2D_Arrays.Pointer;
    end record
        with Convention => C;

    type Pipeline_Discard_Rectangle_State_Create_Info_C_Access is
        access Pipeline_Discard_Rectangle_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Conservative_Rasterization_Properties_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Conservative_Rasterization_Properties_Type;
        Next: C.Out_Structure_C_Access;
        Primitive_Overestimation_Size: Interfaces.C.C_Float;
        Max_Extra_Primitive_Overestimation_Size: Interfaces.C.C_Float;
        Extra_Primitive_Overestimation_Size_Granularity: Interfaces.C.C_Float;
        Primitive_Underestimation: Interfaces.Unsigned_32;
        Conservative_Point_And_Line_Rasterization: Interfaces.Unsigned_32;
        Degenerate_Triangles_Rasterized: Interfaces.Unsigned_32;
        Degenerate_Lines_Rasterized: Interfaces.Unsigned_32;
        Fully_Covered_Fragment_Shader_Input_Variable: Interfaces.Unsigned_32;
        Conservative_Rasterization_Post_Depth_Coverage: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Conservative_Rasterization_Properties_C_Access is
        access Physical_Device_Conservative_Rasterization_Properties_C
        with Convention => C;

    type Pipeline_Rasterization_Conservative_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_Conservative_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags:
          Extensions.EXT.Pipeline_Rasterization_Conservative_State_Create_Flags;
        Conservative_Rasterization_Mode:
            Extensions.EXT.Conservative_Rasterization_Mode;
        Extra_Primitive_Overestimation_Size: Interfaces.C.C_Float;
    end record
        with Convention => C;

    type Pipeline_Rasterization_Conservative_State_Create_Info_C_Access is
        access Pipeline_Rasterization_Conservative_State_Create_Info_C
        with Convention => C;

    type Physical_Device_Depth_Clip_Enable_Features_C is
    record
        Record_Type: Out_Structure_Type :=
            Physical_Device_Depth_Clip_Enable_Features_Type;
        Next: C.Out_Structure_C_Access;
        Depth_Clip_Enable: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Physical_Device_Depth_Clip_Enable_Features_C_Access is
        access Physical_Device_Depth_Clip_Enable_Features_C
        with Convention => C;

    type Pipeline_Rasterization_Depth_Clip_State_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags:
            Extensions.EXT.Pipeline_Rasterization_Depth_Clip_State_Create_Flags;
        Depth_Clip_Enable: Interfaces.Unsigned_32;
    end record
        with Convention => C;

    type Pipeline_Rasterization_Depth_Clip_State_Create_Info_C_Access is
        access Pipeline_Rasterization_Depth_Clip_State_Create_Info_C
        with Convention => C;

    type HDR_Metadata_C is
    record
        Record_Type: In_Structure_Type := HDR_Metadata_Type;
        Next: C.In_Structure_C_Access;
        Display_Primary_Red: Extensions.EXT.XY_Color;
        Display_Primary_Green: Extensions.EXT.XY_Color;
        Display_Primary_Blue: Extensions.EXT.XY_Color;
        White_Point: Extensions.EXT.XY_Color;
        Max_Luminance: Interfaces.C.C_float;
        Min_Luminance: Interfaces.C.C_float;
        Max_Content_Light_Level: Interfaces.C.C_float;
        Max_Frame_Average_Light_Level: Interfaces.C.C_float;
    end record
        with Convention => C;

    type HDR_Metadata_C_Access is access HDR_Metadata_C
        with Convention => C;

    type Debug_Utils_Label_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Label_Type;
        Next: C.In_Structure_C_Access;
        Label_Name: Interfaces.C.Strings.chars_ptr;
        Color: Extensions.EXT.Debug_Color;
    end record
        with Convention => C;

    type Debug_Utils_Label_C_Access is access Debug_Utils_Label_C
        with Convention => C;

    type Debug_Utils_Object_Name_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Object_Name_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Object_Name: Interfaces.C.Strings.chars_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Object_Name_Info_C_Access is
        access Debug_Utils_Object_Name_Info_C
        with Convention => C;

    package Debug_Utils_Label_C_Arrays is new C_Arrays(Debug_Utils_Label_C);

    package Debug_Utils_Object_Name_Info_C_Arrays is
        new C_Arrays(Debug_Utils_Object_Name_Info_C);

    type Debug_Utils_Messenger_Callback_Data_C is
    record
        Record_Type: In_Structure_Type :=
            Debug_Utils_Messenger_Callback_Data_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.EXT.Debug_Utils_Messenger_Callback_Data_Flags;
        Message_ID_Name: Interfaces.C.Strings.chars_ptr;
        Message_ID_Number: Interfaces.Integer_32;
        Message: Interfaces.C.Strings.chars_ptr;
        Queue_Label_Count: Interfaces.Unsigned_32;
        Queue_Labels: Debug_Utils_Label_C_Arrays.Pointer;
        Cmd_Buf_Label_Count: Interfaces.Unsigned_32;
        Cmd_Buf_Labels: Debug_Utils_Label_C_Arrays.Pointer;
        Object_Count: Interfaces.Unsigned_32;
        Objects: Debug_Utils_Object_Name_Info_C_Arrays.Pointer;
    end record
        with Convention => C;

    type Debug_Utils_Messenger_Callback_Data_C_Access is
        access Debug_Utils_Messenger_Callback_Data_C
        with Convention => C;

    type Messenger_Callback_C is
        access function
            (Message_Severity:
                in Extensions.EXT.Debug_Utils_Message_Severity_Flags;
             Message_Types: in Extensions.EXT.Debug_Utils_Message_Type_Flags;
             Callback_Data: in Debug_Utils_Messenger_Callback_Data_C;
             User_Data: in Interfaces.C.Extensions.void_ptr)
        return Interfaces.Unsigned_32
        with Convention => C;

    type Debug_Utils_Messenger_Create_Info_C is
    record
        Record_Type: In_Structure_Type :=
            Debug_Utils_Messenger_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.EXT.Debug_Utils_Messenger_Create_Flags;
        Message_Severity: Extensions.EXT.Debug_Utils_Message_Severity_Flags;
        Message_Type: Extensions.EXT.Debug_Utils_Message_Type_Flags;
        User_Callback: Messenger_Callback_C;
        User_Data: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Messenger_Create_Info_C_Access is
        access all Debug_Utils_Messenger_Create_Info_C
        with Convention => C;

    type Debug_Utils_Object_Tag_Info_C is
    record
        Record_Type: In_Structure_Type := Debug_Utils_Object_Tag_Info_Type;
        Next: C.In_Structure_C_Access;
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Tag_Name: Interfaces.Unsigned_64;
        Tag_Size: Interfaces.C.size_t;
        Tag: Interfaces.C.Extensions.void_ptr;
    end record
        with Convention => C;

    type Debug_Utils_Object_Tag_Info_C_Access is
        access all Debug_Utils_Object_Tag_Info_C
        with Convention => C;

    type Metal_Surface_Create_Info_C is
    record
        Record_Type: In_Structure_Type := Metal_Surface_Create_Info_Type;
        Next: C.In_Structure_C_Access;
        Flags: Extensions.EXT.Metal_Surface_Create_Flags;
        Layer: Metal.Layer;
    end record
        with Convention => C;

    type Metal_Surface_Create_Info_C_Access is
        access Metal_Surface_Create_Info_C
        with Convention => C;

    -- Conversion subprograms.
    function To_C
        (Struct:
            in Extensions.EXT.Rendering_Fragment_Density_Map_Attachment_Info)
        return Rendering_Fragment_Density_Map_Attachment_Info_C;
    procedure Free
        (Struct: in out Rendering_Fragment_Density_Map_Attachment_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Transform_Feedback_Features;
         C_Struct: in Physical_Device_Transform_Feedback_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Transform_Feedback_Properties;
         C_Struct: in Physical_Device_Transform_Feedback_Properties_C);

    function To_C
        (Struct:
            in Extensions.EXT.Pipeline_Rasterization_State_Stream_Create_Info)
        return Pipeline_Rasterization_State_Stream_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Stream_Create_Info_C);

    function To_C(Struct: in Extensions.EXT.Image_View_ASTC_Decode_Mode)
        return Image_View_ASTC_Decode_Mode_C;
    procedure Free(Struct: in out Image_View_ASTC_Decode_Mode_C);

    procedure To_Ada
        (Ada_Struct: in out Extensions.EXT.Physical_Device_ASTC_Decode_Features;
         C_Struct: in Physical_Device_ASTC_Decode_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C);

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.EXT.Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C);

    function To_C(Struct: in Extensions.EXT.Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C;
    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C);

    function To_C(Struct: in Extensions.EXT.Conditional_Rendering_Begin_Info)
        return Conditional_Rendering_Begin_Info_C;
    procedure Free(Struct: in out Conditional_Rendering_Begin_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.EXT.Physical_Device_Conditional_Rendering_Features;
         C_Struct: in Physical_Device_Conditional_Rendering_Features_C);

    function To_C
        (Struct:
        in Extensions.EXT.Command_Buffer_Inheritance_Conditional_Rendering_Info)
        return Command_Buffer_Inheritance_Conditional_Rendering_Info_C;
    procedure Free
        (Struct:
            in out Command_Buffer_Inheritance_Conditional_Rendering_Info_C);

    procedure To_Ada(Ada_Struct: in out Extensions.EXT.Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C);

    function To_C(Struct: in Extensions.EXT.Display_Power_Info)
        return Display_Power_Info_C;
    procedure Free(Struct: in out Display_Power_Info_C);
    
    function To_C(Struct: in Extensions.EXT.Device_Event_Info)
        return Device_Event_Info_C;
    procedure Free(Struct: in out Device_Event_Info_C);

    function To_C(Struct: in Extensions.EXT.Display_Event_Info)
        return Display_Event_Info_C;
    procedure Free(Struct: in out Display_Event_Info_C);

    function To_C(Struct: in Extensions.EXT.Swapchain_Counter_Create_Info)
        return Swapchain_Counter_Create_Info_C;
    procedure Free(Struct: in out Swapchain_Counter_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Discard_Rectangle_Properties;
         C_Struct: in Physical_Device_Discard_Rectangle_Properties_C);

    function To_C
        (Struct: in Extensions.EXT.Pipeline_Discard_Rectangle_State_Create_Info)
        return Pipeline_Discard_Rectangle_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Discard_Rectangle_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out
           Extensions.EXT.Physical_Device_Conservative_Rasterization_Properties;
         C_Struct: in Physical_Device_Conservative_Rasterization_Properties_C);

    function To_C
        (Struct:
        in Extensions.EXT.Pipeline_Rasterization_Conservative_State_Create_Info)
        return Pipeline_Rasterization_Conservative_State_Create_Info_C;
    procedure Free
        (Struct:
            in out Pipeline_Rasterization_Conservative_State_Create_Info_C);

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Depth_Clip_Enable_Features;
         C_Struct: in Physical_Device_Depth_Clip_Enable_Features_C);

    function To_C
        (Struct:
          in Extensions.EXT.Pipeline_Rasterization_Depth_Clip_State_Create_Info)
        return Pipeline_Rasterization_Depth_Clip_State_Create_Info_C;
    procedure Free
        (Struct: in out Pipeline_Rasterization_Depth_Clip_State_Create_Info_C);

    function To_C(Struct: in Extensions.EXT.HDR_Metadata) return HDR_Metadata_C;
    procedure Free(Struct: in out HDR_Metadata_C);

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Label)
        return Debug_Utils_Label_C;
    procedure Free(Struct: in out Debug_Utils_Label_C);

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Object_Name_Info)
        return Debug_Utils_Object_Name_Info_C;
    procedure Free(Struct: in out Debug_Utils_Object_Name_Info_C);

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Messenger_Callback_Data)
        return Debug_Utils_Messenger_Callback_Data_C;
    procedure Free(Struct: in out Debug_Utils_Messenger_Callback_Data_C);

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Messenger_Create_Info)
        return Debug_Utils_Messenger_Create_Info_C;
    procedure Free(Struct: in out Debug_Utils_Messenger_Create_Info_C);

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Object_Tag_Info)
        return Debug_Utils_Object_Tag_Info_C;
    procedure Free(Struct: in out Debug_Utils_Object_Tag_Info_C);

    function To_C(Struct: in Extensions.EXT.Metal_Surface_Create_Info)
        return Metal_Surface_Create_Info_C;
    procedure Free(Struct: in out Metal_Surface_Create_Info_C);

    -- Extension record conversion.
    function To_C(Next: in In_Structure_Access) return C.In_Structure_C_Access;
    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access;
    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access);
    procedure Free(Next: in out C.In_Structure_C_Access);
    procedure Free(Next: in out C.Out_Structure_C_Access);
end Vulkan.C_EXT;

