package body Queues is
    protected body Queue is
    entry Insert (Item : in T; Succeeded : out Boolean)
        when True is
begin
    if (Write_Pos + 1) = Read_Pos then
        Succeeded := False;
        return;
    end if;

    Items (Write_Pos) := Item;
    Write_Pos := Write_Pos + 1;
    Succeeded := True;
end Insert;

entry Remove (Item : out T; Succeeded : out Boolean)
    when True is
        begin
            if Write_Pos = Read_Pos then
                Succeeded := False;
                return;
            end if;
            Item := Items (Read_Pos);
            Read_Pos := Read_Pos + 1;
            Succeeded := True;
        end Remove;

        entry Empty (Status : out Boolean)
            when True is
            begin
                Status := Read_Pos = Write_Pos;
            end Empty;
    end Queue;
end Queues;
