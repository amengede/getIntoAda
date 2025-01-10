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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_EXT is
    function To_C
        (Struct:
            in Extensions.EXT.Rendering_Fragment_Density_Map_Attachment_Info)
        return Rendering_Fragment_Density_Map_Attachment_Info_C is
        RFDMAIC: Rendering_Fragment_Density_Map_Attachment_Info_C;
    begin
        RFDMAIC.Next := Extension_Records.To_C(Struct.Next);
        RFDMAIC.Image_View := Struct.Image_View;
        RFDMAIC.Image_Layout := Struct.Image_Layout;

        return RFDMAIC;
    end To_C;

    procedure Free
        (Struct: in out Rendering_Fragment_Density_Map_Attachment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Transform_Feedback_Features;
         C_Struct: in Physical_Device_Transform_Feedback_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Transform_Feedback :=
            Utilities.To_Ada(C_Struct.Transform_Feedback);
        Ada_Struct.Geometry_Streams :=
            Utilities.To_Ada(C_Struct.Geometry_Streams);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Transform_Feedback_Properties;
         C_Struct: in Physical_Device_Transform_Feedback_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Transform_Feedback_Streams :=
            C_Struct.Max_Transform_Feedback_Streams;
        Ada_Struct.Max_Transform_Feedback_Buffers :=
            C_Struct.Max_Transform_Feedback_Buffers;
        Ada_Struct.Max_Transform_Feedback_Buffer_Size :=
            C_Struct.Max_Transform_Feedback_Buffer_Size;
        Ada_Struct.Max_Transform_Feedback_Stream_Data_Size :=
            C_Struct.Max_Transform_Feedback_Stream_Data_Size;
        Ada_Struct.Max_Transform_Feedback_Buffer_Data_Size :=
            C_Struct.Max_Transform_Feedback_Buffer_Data_Size;
        Ada_Struct.Max_Transform_Feedback_Buffer_Data_Stride :=
            C_Struct.Max_Transform_Feedback_Buffer_Data_Stride;
        Ada_Struct.Transform_Feedback_Queries :=
            Utilities.To_Ada(C_Struct.Transform_Feedback_Queries);
        Ada_Struct.Transform_Feedback_Streams_Lines_Triangles := 
            Utilities.To_Ada
                (C_Struct.Transform_Feedback_Streams_Lines_Triangles);
        Ada_Struct.Transform_Feedback_Rasterization_Stream_Select :=
            Utilities.To_Ada
                (C_Struct.Transform_Feedback_Rasterization_Stream_Select);
        Ada_Struct.Transform_Feedback_Draw :=
            Utilities.To_Ada(C_Struct.Transform_Feedback_Draw);
    end To_Ada;

    function To_C
        (Struct:
            in Extensions.EXT.Pipeline_Rasterization_State_Stream_Create_Info)
        return Pipeline_Rasterization_State_Stream_Create_Info_C is
        PRSSCIC: Pipeline_Rasterization_State_Stream_Create_Info_C;
    begin
        PRSSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRSSCIC.Flags := Struct.Flags;
        PRSSCIC.Rasterization_Stream := Struct.Rasterization_Stream;

        return PRSSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Stream_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.Image_View_ASTC_Decode_Mode)
        return Image_View_ASTC_Decode_Mode_C is
        IVADMC: Image_View_ASTC_Decode_Mode_C;
    begin
        IVADMC.Next := Extension_Records.To_C(Struct.Next);
        IVADMC.Decode_Mode := Struct.Decode_Mode;

        return IVADMC;
    end To_C;

    procedure Free(Struct: in out Image_View_ASTC_Decode_Mode_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.EXT.Physical_Device_ASTC_Decode_Features;
         C_Struct: in Physical_Device_ASTC_Decode_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Decode_Mode_Shared_Exponent :=
            Utilities.To_Ada(C_Struct.Decode_Mode_Shared_Exponent);
    end To_Ada;

    function To_C(Struct: in Extensions.EXT.Pipeline_Robustness_Create_Info)
        return Pipeline_Robustness_Create_Info_C is
        PRCIC: Pipeline_Robustness_Create_Info_C;
    begin
        PRCIC.Next := Extension_Records.To_C(Struct.Next);
        PRCIC.Storage_Buffers := Struct.Storage_Buffers;
        PRCIC.Uniform_Buffers := Struct.Uniform_Buffers;
        PRCIC.Vertex_Inputs := Struct.Vertex_Inputs;
        PRCIC.Images := Struct.Images;

        return PRCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Robustness_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Pipeline_Robustness_Features;
         C_Struct: in Physical_Device_Pipeline_Robustness_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Robustness :=
            Utilities.To_Ada(C_Struct.Pipeline_Robustness);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.EXT.Physical_Device_Pipeline_Robustness_Properties;
         C_Struct: in Physical_Device_Pipeline_Robustness_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Default_Robustness_Storage_Buffers :=
            C_Struct.Default_Robustness_Storage_Buffers;
        Ada_Struct.Default_Robustness_Uniform_Buffers :=
            C_Struct.Default_Robustness_Uniform_Buffers;
        Ada_Struct.Default_Robustness_Vertex_Inputs :=
            C_Struct.Default_Robustness_Vertex_Inputs;
        Ada_Struct.Default_Robustness_Images :=
            C_Struct.Default_Robustness_Images;
    end To_Ada;

    function To_C(Struct: in Extensions.EXT.Conditional_Rendering_Begin_Info)
        return Conditional_Rendering_Begin_Info_C is
        CRBIC: Conditional_Rendering_Begin_Info_C;
    begin
        CRBIC.Next := Extension_Records.To_C(Struct.Next);
        CRBIC.Buffer := Struct.Buffer;
        CRBIC.Offset := Struct.Offset;
        CRBIC.Flags := Struct.Flags;

        return CRBIC;
    end To_C;

    procedure Free(Struct: in out Conditional_Rendering_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out 
                Extensions.EXT.Physical_Device_Conditional_Rendering_Features;
         C_Struct: in Physical_Device_Conditional_Rendering_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Conditional_Rendering :=
            Utilities.To_Ada(C_Struct.Conditional_Rendering);
        Ada_Struct.Inherited_Conditional_Rendering :=
            Utilities.To_Ada(C_Struct.Inherited_Conditional_Rendering);
    end To_Ada;

    function To_C
        (Struct:
        in Extensions.EXT.Command_Buffer_Inheritance_Conditional_Rendering_Info)
        return Command_Buffer_Inheritance_Conditional_Rendering_Info_C is
        CBICRIC: Command_Buffer_Inheritance_Conditional_Rendering_Info_C;
    begin
        CBICRIC.Next := Extension_Records.To_C(Struct.Next);
        CBICRIC.Conditional_Rendering_Enable :=
            Utilities.To_C(Struct.Conditional_Rendering_Enable);

        return CBICRIC;
    end To_C;

    procedure Free
        (Struct:
            in out Command_Buffer_Inheritance_Conditional_Rendering_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Extensions.EXT.Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Min_Image_Count := C_Struct.Min_Image_Count;
        Ada_Struct.Max_Image_Count := C_Struct.Max_Image_Count;
        Ada_Struct.Current_Extent := C_Struct.Current_Extent;
        Ada_Struct.Min_Image_Extent := C_Struct.Min_Image_Extent;
        Ada_Struct.Max_Image_Extent := C_Struct.Max_Image_Extent;
        Ada_Struct.Max_Image_Array_Layers := C_Struct.Max_Image_Array_Layers;
        Ada_Struct.Supported_Transforms := C_Struct.Supported_Transforms;
        Ada_Struct.Current_Transform := C_Struct.Current_Transform;
        Ada_Struct.Supported_Composite_Alpha :=
            C_Struct.Supported_Composite_Alpha;
        Ada_Struct.Supported_Usage_Flags := C_Struct.Supported_Usage_Flags;
        Ada_Struct.Supported_Surface_Counters :=
            C_Struct.Supported_Surface_Counters;
    end To_Ada;

    function To_C(Struct: in Extensions.EXT.Display_Power_Info)
        return Display_Power_Info_C is
        DPIC: Display_Power_Info_C;
    begin
        DPIC.Next := Extension_Records.To_C(Struct.Next);
        DPIC.Power_State := Struct.Power_State;

        return DPIC;
    end To_C;

    procedure Free(Struct: in out Display_Power_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;
    
    function To_C(Struct: in Extensions.EXT.Device_Event_Info)
        return Device_Event_Info_C is
        DEIC: Device_Event_Info_C;
    begin
        DEIC.Next := Extension_Records.To_C(Struct.Next);
        DEIC.Device_Event := Struct.Device_Event;

        return DEIC;
    end To_C;

    procedure Free(Struct: in out Device_Event_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.Display_Event_Info)
        return Display_Event_Info_C is
        DEIC: Display_Event_Info_C;
    begin
        DEIC.Next := Extension_Records.To_C(Struct.Next);
        DEIC.Display_Event := Struct.Display_Event;

        return DEIC;
    end To_C;

    procedure Free(Struct: in out Display_Event_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.Swapchain_Counter_Create_Info)
        return Swapchain_Counter_Create_Info_C is
        SCCIC: Swapchain_Counter_Create_Info_C;
    begin
        SCCIC.Next := Extension_Records.To_C(Struct.Next);
        SCCIC.Surface_Counters := Struct.Surface_Counters;

        return SCCIC;
    end To_C;

    procedure Free(Struct: in out Swapchain_Counter_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Discard_Rectangle_Properties;
         C_Struct: in Physical_Device_Discard_Rectangle_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Discard_Rectangles := C_Struct.Max_Discard_Rectangles;
    end To_Ada;

    function To_C
        (Struct: in Extensions.EXT.Pipeline_Discard_Rectangle_State_Create_Info)
        return Pipeline_Discard_Rectangle_State_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Rect_2D_Arrays,
                                                         Rect_2D_Vectors);

        PDRSCIC: Pipeline_Discard_Rectangle_State_Create_Info_C;
    begin
        PDRSCIC.Next := Extension_Records.To_C(Struct.Next);
        PDRSCIC.Flags := Struct.Flags;
        PDRSCIC.Discard_Rectangle_Mode := Struct.Discard_Rectangle_Mode;
        To_C_Array(PDRSCIC.Discard_Rectangle_Count,
                   Struct.Discard_Rectangles,
                   PDRSCIC.Discard_Rectangles);

        return PDRSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Discard_Rectangle_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Rect_2D_Arrays.Free(Struct.Discard_Rectangles);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out
           Extensions.EXT.Physical_Device_Conservative_Rasterization_Properties;
         C_Struct:
            in Physical_Device_Conservative_Rasterization_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Primitive_Overestimation_Size :=
            Float(C_Struct.Primitive_Overestimation_Size);
        Ada_Struct.Max_Extra_Primitive_Overestimation_Size :=
            Float(C_Struct.Max_Extra_Primitive_Overestimation_Size);
        Ada_Struct.Extra_Primitive_Overestimation_Size_Granularity :=
            Float(C_Struct.Extra_Primitive_Overestimation_Size_Granularity);
        Ada_Struct.Primitive_Underestimation :=
            Utilities.To_Ada(C_Struct.Primitive_Underestimation);
        Ada_Struct.Conservative_Point_And_Line_Rasterization :=
            Utilities.To_Ada
                (C_Struct.Conservative_Point_And_Line_Rasterization);
        Ada_Struct.Degenerate_Triangles_Rasterized :=
            Utilities.To_Ada(C_Struct.Degenerate_Triangles_Rasterized);
        Ada_Struct.Degenerate_Lines_Rasterized :=
            Utilities.To_Ada(C_Struct.Degenerate_Lines_Rasterized);
        Ada_Struct.Fully_Covered_Fragment_Shader_Input_Variable :=
            Utilities.To_Ada
                (C_Struct.Fully_Covered_Fragment_Shader_Input_Variable);
        Ada_Struct.Conservative_Rasterization_Post_Depth_Coverage :=
            Utilities.To_Ada
                (C_Struct.Conservative_Rasterization_Post_Depth_Coverage);
    end To_Ada;

    function To_C
        (Struct:
        in Extensions.EXT.Pipeline_Rasterization_Conservative_State_Create_Info)
        return Pipeline_Rasterization_Conservative_State_Create_Info_C is
        PRCSCIC: Pipeline_Rasterization_Conservative_State_Create_Info_C;
    begin
        PRCSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRCSCIC.Flags := Struct.Flags;
        PRCSCIC.Conservative_Rasterization_Mode :=
            Struct.Conservative_Rasterization_Mode;
        PRCSCIC.Extra_Primitive_Overestimation_Size :=
            Interfaces.C.C_Float(Struct.Extra_Primitive_Overestimation_Size);

        return PRCSCIC;
    end To_C;

    procedure Free
        (Struct:
            in out Pipeline_Rasterization_Conservative_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.EXT.Physical_Device_Depth_Clip_Enable_Features;
         C_Struct: in Physical_Device_Depth_Clip_Enable_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Depth_Clip_Enable :=
            Utilities.To_Ada(C_Struct.Depth_Clip_Enable);
    end To_Ada;

    function To_C
        (Struct:
          in Extensions.EXT.Pipeline_Rasterization_Depth_Clip_State_Create_Info)
        return Pipeline_Rasterization_Depth_Clip_State_Create_Info_C is
        PRDCSCIC: Pipeline_Rasterization_Depth_Clip_State_Create_Info_C;
    begin
        PRDCSCIC.Next := Extension_Records.To_C(Struct.Next);
        PRDCSCIC.Flags := Struct.Flags;
        PRDCSCIC.Depth_Clip_Enable := Utilities.To_C(Struct.Depth_Clip_Enable);

        return PRDCSCIC;
    end To_C;

    procedure Free
        (Struct:
            in out Pipeline_Rasterization_Depth_Clip_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.HDR_Metadata)
        return HDR_Metadata_C is
        HMC: HDR_Metadata_C;
    begin
        HMC.Next := Extension_Records.To_C(Struct.Next);
        HMC.Display_Primary_Red := Struct.Display_Primary_Red;
        HMC.Display_Primary_Green := Struct.Display_Primary_Green;
        HMC.Display_Primary_Blue := Struct.Display_Primary_Blue;
        HMC.White_Point := Struct.White_Point;
        HMC.Max_Luminance := Interfaces.C.C_float(Struct.Max_Luminance);
        HMC.Min_Luminance := Interfaces.C.C_float(Struct.Min_Luminance);
        HMC.Max_Content_Light_Level :=
            Interfaces.C.C_float(Struct.Max_Content_Light_Level);
        HMC.Max_Frame_Average_Light_Level :=
            Interfaces.C.C_float(Struct.Max_Frame_Average_Light_Level);

        return HMC;
    end To_C;

    procedure Free(Struct: in out HDR_Metadata_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Label)
        return Debug_Utils_Label_C is
        DULC: Debug_Utils_Label_C;
    begin
        DULC.Next := Extension_Records.To_C(Struct.Next);
        DULC.Label_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Label_Name));
        DULC.Color := Struct.Color;

        return DULC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Label_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Label_Name);
    end Free;

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Object_Name_Info)
        return Debug_Utils_Object_Name_Info_C is
        DUONIC: Debug_Utils_Object_Name_Info_C;
    begin
        DUONIC.Next := Extension_Records.To_C(Struct.Next);
        DUONIC.Object_Type := Struct.Object_Type;
        DUONIC.Object_Handle := Struct.Object_Handle;
        DUONIC.Object_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Object_Name));

        return DUONIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Object_Name_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Object_Name);
    end Free;

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Messenger_Callback_Data)
        return Debug_Utils_Messenger_Callback_Data_C is

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Debug_Utils_Label_C_Arrays,
             Extensions.EXT.Debug_Utils_Label_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Debug_Utils_Object_Name_Info_C_Arrays,
             Extensions.EXT.Debug_Utils_Object_Name_Info_Vectors);

        DUMCDC: Debug_Utils_Messenger_Callback_Data_C;
    begin
        DUMCDC.Next := Extension_Records.To_C(Struct.Next);
        DUMCDC.Flags := Struct.Flags;
        DUMCDC.Message_ID_Name := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Message_ID_Name));
        DUMCDC.Message_ID_Number := Struct.Message_ID_Number;
        DUMCDC.Message := Interfaces.C.Strings.New_String
            (Ada.Strings.Unbounded.To_String(Struct.Message));
        To_C_Array(DUMCDC.Queue_Label_Count,
                   Struct.Queue_Labels,
                   DUMCDC.Queue_Labels);
        To_C_Array(DUMCDC.Cmd_Buf_Label_Count,
                   Struct.Cmd_Buf_Labels,
                   DUMCDC.Cmd_Buf_Labels);
        To_C_Array(DUMCDC.Object_Count, Struct.Objects, DUMCDC.Objects);

        return DUMCDC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Messenger_Callback_Data_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Interfaces.C.Strings.Free(Struct.Message_ID_Name);
        Interfaces.C.Strings.Free(Struct.Message);
        Debug_Utils_Label_C_Arrays.Free(Struct.Queue_Labels, Free'Access);
        Debug_Utils_Label_C_Arrays.Free(Struct.Cmd_Buf_Labels, Free'Access);
        Debug_Utils_Object_Name_Info_C_Arrays.Free(Struct.Objects, Free'Access);
    end Free;

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Messenger_Create_Info)
        return Debug_Utils_Messenger_Create_Info_C is
        DUMCIC: Debug_Utils_Messenger_Create_Info_C;
    begin
        DUMCIC.Next := Extension_Records.To_C(Struct.Next);
        DUMCIC.Flags := Struct.Flags;
        DUMCIC.Message_Severity := Struct.Message_Severity;
        DUMCIC.Message_Type := Struct.Message_Type;

        return DUMCIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Messenger_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.EXT.Debug_Utils_Object_Tag_Info)
        return Debug_Utils_Object_Tag_Info_C is
        DUOTIC: Debug_Utils_Object_Tag_Info_C;
    begin
        DUOTIC.Next := Extension_Records.To_C(Struct.Next);
        DUOTIC.Object_Type := Struct.Object_Type;
        DUOTIC.Object_Handle := Struct.Object_Handle;
        DUOTIC.Tag_Name := Struct.Tag_Name;
        DUOTIC.Tag_Size := Struct.Tag_Size;
        DUOTIC.Tag := Struct.Tag;

        return DUOTIC;
    end To_C;

    procedure Free(Struct: in out Debug_Utils_Object_Tag_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free; 

    function To_C(Struct: in Extensions.EXT.Metal_Surface_Create_Info)
        return Metal_Surface_Create_Info_C is
        MSCIC: Metal_Surface_Create_Info_C;
    begin
        MSCIC.Next := Extension_Records.To_C(Struct.Next);
        MSCIC.Flags := Struct.Flags;
        MSCIC.Layer := Struct.Layer;

        return MSCIC;
    end To_C;

    procedure Free(Struct: in out Metal_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Rendering_Fragment_Density_Map_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                 (Extensions.EXT.Rendering_Fragment_Density_Map_Attachment_Info,
                  Rendering_Fragment_Density_Map_Attachment_Info_C,
                  Rendering_Fragment_Density_Map_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Stream_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                (Extensions.EXT.Pipeline_Rasterization_State_Stream_Create_Info,
                 Pipeline_Rasterization_State_Stream_Create_Info_C,
                 Pipeline_Rasterization_State_Stream_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_View_ASTC_Decode_Mode_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Image_View_ASTC_Decode_Mode,
                         Image_View_ASTC_Decode_Mode_C,
                         Image_View_ASTC_Decode_Mode_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Pipeline_Robustness_Create_Info,
                         Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Conditional_Rendering_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Conditional_Rendering_Begin_Info,
                         Conditional_Rendering_Begin_Info_C,
                         Conditional_Rendering_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Conditional_Rendering_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
          (Extensions.EXT.Command_Buffer_Inheritance_Conditional_Rendering_Info,
           Command_Buffer_Inheritance_Conditional_Rendering_Info_C,
           Command_Buffer_Inheritance_Conditional_Rendering_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Power_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Display_Power_Info,
                         Display_Power_Info_C,
                         Display_Power_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Event_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Device_Event_Info,
                         Device_Event_Info_C,
                         Device_Event_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Event_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Display_Event_Info,
                         Display_Event_Info_C,
                         Display_Event_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Swapchain_Counter_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Swapchain_Counter_Create_Info,
                         Swapchain_Counter_Create_Info_C,
                         Swapchain_Counter_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Discard_Rectangle_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                   (Extensions.EXT.Pipeline_Discard_Rectangle_State_Create_Info,
                    Pipeline_Discard_Rectangle_State_Create_Info_C,
                    Pipeline_Discard_Rectangle_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_Conservative_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
          (Extensions.EXT.Pipeline_Rasterization_Conservative_State_Create_Info,
           Pipeline_Rasterization_Conservative_State_Create_Info_C,
           Pipeline_Rasterization_Conservative_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
            (Extensions.EXT.Pipeline_Rasterization_Depth_Clip_State_Create_Info,
             Pipeline_Rasterization_Depth_Clip_State_Create_Info_C,
             Pipeline_Rasterization_Depth_Clip_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when HDR_Metadata_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.HDR_Metadata,
                         HDR_Metadata_C,
                         HDR_Metadata_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Label_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Debug_Utils_Label,
                         Debug_Utils_Label_C,
                         Debug_Utils_Label_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Object_Name_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Debug_Utils_Object_Name_Info,
                         Debug_Utils_Object_Name_Info_C,
                         Debug_Utils_Object_Name_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Messenger_Callback_Data_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Debug_Utils_Messenger_Callback_Data,
                         Debug_Utils_Messenger_Callback_Data_C,
                         Debug_Utils_Messenger_Callback_Data_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Messenger_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Debug_Utils_Messenger_Create_Info,
                         Debug_Utils_Messenger_Create_Info_C,
                         Debug_Utils_Messenger_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Debug_Utils_Object_Tag_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Debug_Utils_Object_Tag_Info,
                         Debug_Utils_Object_Tag_Info_C,
                         Debug_Utils_Object_Tag_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Metal_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.EXT.Metal_Surface_Create_Info,
                         Metal_Surface_Create_Info_C,
                         Metal_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Extensions.EXT.Physical_Device_Transform_Feedback_Features,
                     Physical_Device_Transform_Feedback_Features_C,
                     Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                  (Extensions.EXT.Physical_Device_Transform_Feedback_Properties,
                   Physical_Device_Transform_Feedback_Properties_C,
                   Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_ASTC_Decode_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.EXT.Physical_Device_ASTC_Decode_Features,
                         Physical_Device_ASTC_Decode_Features_C,
                         Physical_Device_ASTC_Decode_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.EXT.Physical_Device_Pipeline_Robustness_Features,
                    Physical_Device_Pipeline_Robustness_Features_C,
                    Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Extensions.EXT.Physical_Device_Pipeline_Robustness_Properties,
                  Physical_Device_Pipeline_Robustness_Properties_C,
                  Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Extensions.EXT.Physical_Device_Conditional_Rendering_Features,
                  Physical_Device_Conditional_Rendering_Features_C,
                  Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Surface_Capabilities_2_EXT_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.EXT.Surface_Capabilities_2,
                         Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Discard_Rectangle_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.EXT.Physical_Device_Discard_Rectangle_Properties,
                    Physical_Device_Discard_Rectangle_Properties_C,
                    Physical_Device_Discard_Rectangle_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Conservative_Rasterization_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
          (Extensions.EXT.Physical_Device_Conservative_Rasterization_Properties,
           Physical_Device_Conservative_Rasterization_Properties_C,
           Physical_Device_Conservative_Rasterization_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Depth_Clip_Enable_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Extensions.EXT.Physical_Device_Depth_Clip_Enable_Features,
                      Physical_Device_Depth_Clip_Enable_Features_C,
                      Physical_Device_Depth_Clip_Enable_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    To_Ada
                     (Extensions.EXT.Physical_Device_Transform_Feedback_Features
                         (Ada_Struct),
                      To_Access(Next).all);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    To_Ada
                   (Extensions.EXT.Physical_Device_Transform_Feedback_Properties
                       (Ada_Struct),
                    To_Access(Next).all);
                end;
            when Physical_Device_ASTC_Decode_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_ASTC_Decode_Features_C_Access);
                begin
                    To_Ada(Extensions.EXT.Physical_Device_ASTC_Decode_Features
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    To_Ada
                    (Extensions.EXT.Physical_Device_Pipeline_Robustness_Features
                        (Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    To_Ada
                  (Extensions.EXT.Physical_Device_Pipeline_Robustness_Properties
                      (Ada_Struct),
                   To_Access(Next).all);
                end;
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    To_Ada
                  (Extensions.EXT.Physical_Device_Conditional_Rendering_Features
                      (Ada_Struct),
                   To_Access(Next).all);
                end;
            when Surface_Capabilities_2_EXT_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Surface_Capabilities_2_C_Access);
                begin
                    To_Ada(Extensions.EXT.Surface_Capabilities_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Discard_Rectangle_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Discard_Rectangle_Properties_C_Access);
                begin
                    To_Ada
                    (Extensions.EXT.Physical_Device_Discard_Rectangle_Properties
                        (Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Conservative_Rasterization_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
               (C.Out_Structure_C_Access,
                Physical_Device_Conservative_Rasterization_Properties_C_Access);
                begin
                    To_Ada
           (Extensions.EXT.Physical_Device_Conservative_Rasterization_Properties
               (Ada_Struct),
            To_Access(Next).all);
                end;
            when Physical_Device_Depth_Clip_Enable_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Depth_Clip_Enable_Features_C_Access);
                begin
                  To_Ada
                      (Extensions.EXT.Physical_Device_Depth_Clip_Enable_Features
                          (Ada_Struct),
                       To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Rendering_Fragment_Density_Map_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                      (Rendering_Fragment_Density_Map_Attachment_Info_C,
                       Rendering_Fragment_Density_Map_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_State_Stream_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Pipeline_Rasterization_State_Stream_Create_Info_C,
                      Pipeline_Rasterization_State_Stream_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_View_ASTC_Decode_Mode_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_View_ASTC_Decode_Mode_C,
                         Image_View_ASTC_Decode_Mode_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Robustness_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Robustness_Create_Info_C,
                         Pipeline_Robustness_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Conditional_Rendering_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Conditional_Rendering_Begin_Info_C,
                         Conditional_Rendering_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Command_Buffer_Inheritance_Conditional_Rendering_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
               (Command_Buffer_Inheritance_Conditional_Rendering_Info_C,
                Command_Buffer_Inheritance_Conditional_Rendering_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Power_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Power_Info_C, Display_Power_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Event_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Event_Info_C, Device_Event_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Event_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Event_Info_C, Display_Event_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Swapchain_Counter_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Swapchain_Counter_Create_Info_C,
                         Swapchain_Counter_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Discard_Rectangle_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Discard_Rectangle_State_Create_Info_C,
                         Pipeline_Discard_Rectangle_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_Conservative_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
               (Pipeline_Rasterization_Conservative_State_Create_Info_C,
                Pipeline_Rasterization_Conservative_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Rasterization_Depth_Clip_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                 (Pipeline_Rasterization_Depth_Clip_State_Create_Info_C,
                  Pipeline_Rasterization_Depth_Clip_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when HDR_Metadata_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (HDR_Metadata_C, HDR_Metadata_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Label_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Label_C,
                         Debug_Utils_Label_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Object_Name_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Object_Name_Info_C,
                         Debug_Utils_Object_Name_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Messenger_Callback_Data_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Messenger_Callback_Data_C,
                         Debug_Utils_Messenger_Callback_Data_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Messenger_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Messenger_Create_Info_C,
                         Debug_Utils_Messenger_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Debug_Utils_Object_Tag_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Debug_Utils_Object_Tag_Info_C,
                         Debug_Utils_Object_Tag_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Metal_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Metal_Surface_Create_Info_C,
                         Metal_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Transform_Feedback_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Transform_Feedback_Features_C,
                         Physical_Device_Transform_Feedback_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Transform_Feedback_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Transform_Feedback_Properties_C,
                        Physical_Device_Transform_Feedback_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_ASTC_Decode_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_ASTC_Decode_Features_C,
                         Physical_Device_ASTC_Decode_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Pipeline_Robustness_Features_C,
                         Physical_Device_Pipeline_Robustness_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Robustness_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Pipeline_Robustness_Properties_C,
                       Physical_Device_Pipeline_Robustness_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Conditional_Rendering_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Conditional_Rendering_Features_C,
                       Physical_Device_Conditional_Rendering_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Surface_Capabilities_2_EXT_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Discard_Rectangle_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Discard_Rectangle_Properties_C,
                         Physical_Device_Discard_Rectangle_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Conservative_Rasterization_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
               (Physical_Device_Conservative_Rasterization_Properties_C,
                Physical_Device_Conservative_Rasterization_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Depth_Clip_Enable_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Depth_Clip_Enable_Features_C,
                         Physical_Device_Depth_Clip_Enable_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_EXT;

