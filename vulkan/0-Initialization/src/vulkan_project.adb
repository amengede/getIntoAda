with Vulkan;
with Vulkan.Core;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Vulkan_Project is

   use type Vulkan.Result;

   App_Info : aliased Vulkan.Application_Info;
   Create_Info : Vulkan.Instance_Create_Info;
   Result : Vulkan.Result;
   Instance : Vulkan.Instance;
   Version : vulkan.Version_Number;
   Features : Vulkan.Physical_Device_Features;
   Layers : Vulkan.Layer_Properties_Vectors.Vector :=
      Vulkan.Core.Enumerate_Instance_Layer_Properties;

begin
   
   for Layer of Layers loop
      Put_Line (To_String (Layer.Name));
   end loop;

   Version := Vulkan.Create_Version (
      Major => 1, 
      Minor => 3, 
      Patch => 0);

   App_Info.API_Version := Version;
   App_Info.Application_Name := To_Unbounded_String ("Quakers");
   App_Info.Application_Version := Version;
   App_Info.Engine_Name := To_Unbounded_String ("Handmade");
   App_Info.Engine_Version := Version;

   Create_Info.Application_Info := App_Info'Unchecked_Access;

   Result := Vulkan.Core.Create(Create_Info, Instance);

   if Result = Vulkan.Success then
      Ada.Text_IO.Put_Line ("Success!");
   else
      Ada.Text_IO.Put_Line ("Failure!");
   end if;
end Vulkan_Project;
