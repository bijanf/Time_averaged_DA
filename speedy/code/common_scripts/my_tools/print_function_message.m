function print_function_message(message_kind,minimum_verbosity,current_verbosity)
    [ST,I] = dbstack;

    if(nargin==1)
        current_verbosity = 1;
        minimum_verbosity = 1;
    end
        
    if(current_verbosity >= minimum_verbosity)
        
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
    end
end
