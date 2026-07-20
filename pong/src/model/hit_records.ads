package Hit_Records is

    type Hit_Record is
        record
            Horizontal : Boolean := False;
            Vertical : Boolean := False;
        end record;
    --  Describes the result of a collision
    --  @field Horizontal Whether a collision
    --  resulted from horizontal movement
    --  @field Horizontal Whether a collision
    --  resulted from Vertical movement

end Hit_Records;
