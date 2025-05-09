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

-- General extensions root package

with Vulkan.Extensions.KHR;
with Vulkan.Metal;

package Vulkan.Extensions.EXT is
    -- Handle types.
    type Debug_Utils_Messenger is new Object_Handle;

    No_Debug_Utils_Messenger: constant Debug_Utils_Messenger :=
        Debug_Utils_Messenger(System.Null_Address);

    -- Enumerations. 
    type Display_Power_State is (Off,
                                 Suspend,
                                 On)
        with Convention => C;

    for Display_Power_State'Size use 32;

    for Display_Power_State use (Off => 0,
                                 Suspend => 1,
                                 On => 2);

    type Device_Event_Type is (Display_Hotplug)
        with Convention => C;

    for Device_Event_Type'Size use 32;

    for Device_Event_Type use (Display_Hotplug => 0);

    type Display_Event_Type is (First_Pixel_Out)
        with Convention => C;

    for Display_Event_Type'Size use 32;

    for Display_Event_Type use (First_Pixel_Out => 0);

    type Discard_Rectangle_Mode is (Inclusive,
                                    Exclusive)
        with Convention => C;

    for Discard_Rectangle_Mode'Size use 32;

    for Discard_Rectangle_Mode use (Inclusive => 0,
                                    Exclusive => 1);

    type Conservative_Rasterization_Mode is (Disabled,
                                             Overestimate,
                                             Underestimate)
        with Convention => C;

    for Conservative_Rasterization_Mode'Size use 32;

    for Conservative_Rasterization_Mode use (Disabled => 0,
                                             Overestimate => 1,
                                             Underestimate => 2);

    -- Bitfields.
    type Pipeline_Rasterization_State_Stream_Create_Flags is new Flags;

    Pipeline_Rasterization_State_Stream_Create_No_Bit:
        constant Pipeline_Rasterization_State_Stream_Create_Flags := 0;

    type Conditional_Rendering_Flags is new Flags;

    Conditional_Rendering_No_Bit: constant Conditional_Rendering_Flags := 0;
    Conditional_Rendering_Inverted_Bit:
        constant Conditional_Rendering_Flags := 16#00000001#;

    type Surface_Counter_Flags is new Flags;

    Surface_Counter_No_Bit: constant Surface_Counter_Flags := 0;
    Surface_Counter_VBlank_Bit: constant Surface_Counter_Flags := 16#00000001#;

    type Pipeline_Discard_Rectangle_State_Create_Flags is new Flags;

    Pipeline_Discard_Rectangle_State_Create_No_Bit:
        constant Pipeline_Discard_Rectangle_State_Create_Flags := 0;

    type Pipeline_Rasterization_Conservative_State_Create_Flags is new Flags;

    Pipeline_Rasterization_Conservative_State_Create_No_Bit:
        constant Pipeline_Rasterization_Conservative_State_Create_Flags := 0;

    type Pipeline_Rasterization_Depth_Clip_State_Create_Flags is new Flags;

    Pipeline_Rasterization_Depth_Clip_State_Create_No_Bit:
        constant Pipeline_Rasterization_Depth_Clip_State_Create_Flags := 0;

    type Debug_Utils_Messenger_Callback_Data_Flags is new Flags;

    Debug_Utils_Messenger_Callback_Data_No_Bit:
        constant Debug_Utils_Messenger_Callback_Data_Flags := 0;

    type Debug_Utils_Message_Severity_Flags is new Flags;

    Debug_Utils_Message_Severity_No_Bit:
        constant Debug_Utils_Message_Severity_Flags := 0;
    Debug_Utils_Message_Severity_Verbose_Bit:
        constant Debug_Utils_Message_Severity_Flags := 16#00000001#;
    Debug_Utils_Message_Severity_Info_Bit:
        constant Debug_Utils_Message_Severity_Flags := 16#00000010#;
    Debug_Utils_Message_Severity_Warning_Bit:
        constant Debug_Utils_Message_Severity_Flags := 16#00000100#;
    Debug_Utils_Message_Severity_Error_Bit:
        constant Debug_Utils_Message_Severity_Flags := 16#00001000#;

    type Debug_Utils_Message_Type_Flags is new Flags;

    Debug_Utils_Message_Type_No_Bit:
        constant Debug_Utils_Message_Type_Flags := 0;
    Debug_Utils_Message_Type_General_Bit:
        constant Debug_Utils_Message_Type_Flags := 16#00000001#;
    Debug_Utils_Message_Type_Validation_Bit:
        constant Debug_Utils_Message_Type_Flags := 16#00000002#;
    Debug_Utils_Message_Type_Performance_Bit:
        constant Debug_Utils_Message_Type_Flags := 16#00000004#;

    type Debug_Utils_Messenger_Create_Flags is new Flags;

    Debug_Utils_Messenger_Create_No_Bit:
        constant Debug_Utils_Messenger_Create_Flags := 0;

    type Metal_Surface_Create_Flags is new Flags;

    Metal_Surface_Create_No_Bit: constant Metal_Surface_Create_Flags := 0;

    -- Records.
    type Rendering_Fragment_Density_Map_Attachment_Info is new In_Structure
        (Rendering_Fragment_Density_Map_Attachment_Info_Type) with
    record
        Image_View: Vulkan.Image_View;
        Image_Layout: Vulkan.Image_Layout;
    end record;

    type Physical_Device_Transform_Feedback_Features is new Out_Structure
        (Physical_Device_Transform_Feedback_Features_Type) with
    record
        Transform_Feedback: Boolean;
        Geometry_Streams: Boolean;
    end record;

    type Physical_Device_Transform_Feedback_Properties is new Out_Structure
        (Physical_Device_Transform_Feedback_Properties_Type) with
    record
        Max_Transform_Feedback_Streams: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffers: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Size: Device_Size;
        Max_Transform_Feedback_Stream_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Size: Interfaces.Unsigned_32;
        Max_Transform_Feedback_Buffer_Data_Stride: Interfaces.Unsigned_32;
        Transform_Feedback_Queries: Boolean;
        Transform_Feedback_Streams_Lines_Triangles: Boolean;
        Transform_Feedback_Rasterization_Stream_Select: Boolean;
        Transform_Feedback_Draw: Boolean;
    end record;

    type Pipeline_Rasterization_State_Stream_Create_Info is new In_Structure
        (Pipeline_Rasterization_State_Stream_Create_Info_Type) with
    record
        Flags: Pipeline_Rasterization_State_Stream_Create_Flags :=
            Pipeline_Rasterization_State_Stream_Create_No_Bit;
            Rasterization_Stream: Interfaces.Unsigned_32;
    end record;

    type Image_View_ASTC_Decode_Mode is new In_Structure
        (Image_View_ASTC_Decode_Mode_Type) with
    record
        Decode_Mode: Format;
    end record;

    type Physical_Device_ASTC_Decode_Features is new Out_Structure
        (Physical_Device_ASTC_Decode_Features_Type) with
    record
        Decode_Mode_Shared_Exponent: Boolean;
    end record;
 
    type Conditional_Rendering_Begin_Info is new In_Structure
        (Conditional_Rendering_Begin_Info_Type) with
    record
        Buffer: Vulkan.Buffer;
        Offset: Device_Size;
        Flags: Conditional_Rendering_Flags := Conditional_Rendering_No_Bit;
    end record;
    
    type Physical_Device_Conditional_Rendering_Features is new Out_Structure
        (Physical_Device_Conditional_Rendering_Features_Type) with
    record
        Conditional_Rendering: Boolean;
        Inherited_Conditional_Rendering: Boolean;
    end record;

    type Command_Buffer_Inheritance_Conditional_Rendering_Info is
        new In_Structure
            (Command_Buffer_Inheritance_Conditional_Rendering_Info_Type) with
    record
        Conditional_Rendering_Enable: Boolean;
    end record;

    type Surface_Capabilities_2 is new Out_Structure
        (Surface_Capabilities_2_EXT_Type) with
    record
        Min_Image_Count: Interfaces.Unsigned_32;
        Max_Image_Count: Interfaces.Unsigned_32;
        Current_Extent: Extent_2D;
        Min_Image_Extent: Extent_2D;
        Max_Image_Extent: Extent_2D;
        Max_Image_Array_Layers: Interfaces.Unsigned_32;
        Supported_Transforms: KHR.Surface_Transform_Flags :=
            KHR.Surface_Transform_No_Bit;
        Current_Transform: KHR.Surface_Transform_Flags :=
            KHR.Surface_Transform_No_Bit;
        Supported_Composite_Alpha: KHR.Composite_Alpha_Flags :=
            KHR.Composite_Alpha_No_Bit;
        Supported_Usage_Flags: Image_Usage_Flags := Image_Usage_No_Bit;
        Supported_Surface_Counters: Surface_Counter_Flags :=
            Surface_Counter_No_Bit;
    end record;

    type Display_Power_Info is new In_Structure(Display_Power_Info_Type) with
    record
        Power_State: Display_Power_State;
    end record;

    type Device_Event_Info is new In_Structure(Device_Event_Info_Type) with
    record
        Device_Event: Device_Event_Type;
    end record;

    type Display_Event_Info is new In_Structure(Display_Event_Info_Type) with
    record
        Display_Event: Display_Event_Type;
    end record;

    type Swapchain_Counter_Create_Info is new In_Structure
        (Swapchain_Counter_Create_Info_Type) with
    record
        Surface_Counters: Surface_Counter_Flags := Surface_Counter_No_Bit;
    end record;

    type Physical_Device_Discard_Rectangle_Properties is new Out_Structure
        (Physical_Device_Discard_Rectangle_Properties_Type) with
    record
        Max_Discard_Rectangles: Interfaces.Unsigned_32;
    end record;

    type Pipeline_Discard_Rectangle_State_Create_Info is new In_Structure
        (Pipeline_Discard_Rectangle_State_Create_Info_Type) with
    record
        Flags: Pipeline_Discard_Rectangle_State_Create_Flags :=
            Pipeline_Discard_Rectangle_State_Create_No_Bit;
        Discard_Rectangle_Mode: EXT.Discard_Rectangle_Mode;
        Discard_Rectangles: Rect_2D_Vectors.Vector;
    end record;

    type Physical_Device_Conservative_Rasterization_Properties is
        new Out_Structure
            (Physical_Device_Conservative_Rasterization_Properties_Type) with
    record
        Primitive_Overestimation_Size: Float;
        Max_Extra_Primitive_Overestimation_Size: Float;
        Extra_Primitive_Overestimation_Size_Granularity: Float;
        Primitive_Underestimation: Boolean;
        Conservative_Point_And_Line_Rasterization: Boolean;
        Degenerate_Triangles_Rasterized: Boolean;
        Degenerate_Lines_Rasterized: Boolean;
        Fully_Covered_Fragment_Shader_Input_Variable: Boolean;
        Conservative_Rasterization_Post_Depth_Coverage: Boolean;
    end record;

    type Pipeline_Rasterization_Conservative_State_Create_Info is
        new In_Structure
            (Pipeline_Rasterization_Conservative_State_Create_Info_Type) with
    record
        Flags: Pipeline_Rasterization_Conservative_State_Create_Flags :=
            Pipeline_Rasterization_Conservative_State_Create_No_Bit;
        Conservative_Rasterization_Mode: EXT.Conservative_Rasterization_Mode;
        Extra_Primitive_Overestimation_Size: Float;
    end record;

    type Physical_Device_Depth_Clip_Enable_Features is new Out_Structure
        (Physical_Device_Depth_Clip_Enable_Features_Type) with
    record
        Depth_Clip_Enable: Boolean;
    end record;

    type Pipeline_Rasterization_Depth_Clip_State_Create_Info is new In_Structure
        (Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type) with
    record
        Flags: Pipeline_Rasterization_Depth_Clip_State_Create_Flags :=
            Pipeline_Rasterization_Depth_Clip_State_Create_No_Bit;
        Depth_Clip_Enable: Boolean;
    end record;

    type XY_Color is
    record
        X: Interfaces.C.C_float;
        Y: Interfaces.C.C_float;
    end record
        with Convention => C;

    type HDR_Metadata is new In_Structure(HDR_Metadata_Type) with
    record
        Display_Primary_Red: XY_Color;
        Display_Primary_Green: XY_Color;
        Display_Primary_Blue: XY_Color;
        White_Point: XY_Color;
        Max_Luminance: Float;
        Min_Luminance: Float;
        Max_Content_Light_Level: Float;
        Max_Frame_Average_Light_Level: Float;
    end record;

    package HDR_Metadata_Vectors is new Ada.Containers.Vectors(Positive,
                                                               HDR_Metadata);

    type Debug_Color is array (1 .. 4) of aliased Interfaces.C.C_float
        with Convention => C; 

    type Debug_Utils_Label is new In_Structure(Debug_Utils_Label_Type) with
    record
        Label_Name: Ada.Strings.Unbounded.Unbounded_String;
        Color: Debug_Color;
    end record;

    type Debug_Utils_Object_Name_Info is
        new In_Structure(Debug_Utils_Object_Name_Info_Type) with
    record
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Object_Name: Ada.Strings.Unbounded.Unbounded_String;
    end record;

    package Debug_Utils_Label_Vectors is
        new Ada.Containers.Vectors(Positive, Debug_Utils_Label);

    package Debug_Utils_Object_Name_Info_Vectors is
        new Ada.Containers.Vectors(Positive, Debug_Utils_Object_Name_Info);

    type Debug_Utils_Messenger_Callback_Data is
        new In_Structure(Debug_Utils_Messenger_Callback_Data_Type) with
    record
        Flags: Debug_Utils_Messenger_Callback_Data_Flags :=
            Debug_Utils_Messenger_Callback_Data_No_Bit;
        Message_ID_Name: Ada.Strings.Unbounded.Unbounded_String;
        Message_ID_Number: Interfaces.Integer_32;
        Message: Ada.Strings.Unbounded.Unbounded_String;
        Queue_Labels: Debug_Utils_Label_Vectors.Vector;
        Cmd_Buf_Labels: Debug_Utils_Label_Vectors.Vector;
        Objects: Debug_Utils_Object_Name_Info_Vectors.Vector;
    end record;

    type Debug_Messenger_Callback is access function
        (Message_Severity: in Debug_Utils_Message_Severity_Flags;
         Message_Types: in Debug_Utils_Message_Type_Flags;
         Callback_Data: in Debug_Utils_Messenger_Callback_Data;
         User_Data: in Interfaces.C.Extensions.void_ptr) return Boolean;

    type Debug_Utils_Messenger_Create_Info is
        new In_Structure(Debug_Utils_Messenger_Create_Info_Type) with
    record
        Flags: Debug_Utils_Messenger_Create_Flags :=
            Debug_Utils_Messenger_Create_No_Bit;
        Message_Severity: Debug_Utils_Message_Severity_Flags;
        Message_Type: Debug_Utils_Message_Type_Flags;
        User_Callback: Debug_Messenger_Callback;
        User_Data: Interfaces.C.Extensions.void_ptr := System.Null_Address;
    end record;

    type Debug_Utils_Object_Tag_Info is
        new In_Structure(Debug_Utils_Object_Tag_Info_Type) with
    record
        Object_Type: Vulkan.Object_Type;
        Object_Handle: Vulkan.Object_Handle;
        Tag_Name: Interfaces.Unsigned_64;
        Tag_Size: Interfaces.C.size_t;
        Tag: Interfaces.C.Extensions.void_ptr;
    end record;

    type Metal_Surface_Create_Info is
        new In_Structure(Metal_Surface_Create_Info_Type) with
    record
        Flags: Metal_Surface_Create_Flags := Metal_Surface_Create_No_Bit;
        Layer: Metal.Layer;
    end record;
end Vulkan.Extensions.EXT;

