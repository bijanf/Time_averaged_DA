function print_function_message(message_kind)
[ST,I] = dbstack;

switch message_kind
    case 'opening'
        print_line
        disp([' ' ST(2).name ' START']);
        print_line
    case 'closing'
        print_line
        disp([' ' ST(2).name ' NORMAL END']);
        print_line
    otherwise
        error(['Unknown message_kind ' message_kind]);
end
