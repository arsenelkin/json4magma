freeze;

/************************************************************************************
 *
 * JSON Parser for MAGMA
 *
 * Author: Arsen Elkin
 * Affiliation: University of Warwick
 *
 ************************************************************************************/
 
forward JSONParse, JSONIsNull, JSONParseObject, JSONParsePair, JSONParseArray,
        JSONParseValue, JSONParseString, JSONParseNumber, SkipWhitespaces, IsEOS, 
        GetChar, JSONTOStringInternal, ThrowUnexpectedToken, JSONPrintInternal;


intrinsic JSONParse(str::MonStgElt) -> .
{ Parse a string in JSON format into an object }
    //printf "Entering JSONParse.\n";
    index := SkipWhitespaces(str, 1);
    character := GetChar(str, index);
    if character eq "{" then
        object, index := JSONParseObject(str, index);
    elif character eq "[" then
        object, index := JSONParseArray(str, index);
    else
        ThrowUnexpectedToken(character, index, "JSONParse");
    end if;

    index := SkipWhitespaces(str, index);
    if not IsEOS(str, index) then
        error "SyntaxError: Unexpected token " cat str[index] cat " at index " cat IntegerToString(index);
    end if;
    //printf "Exiting JSONParse.\n";
    return object;
end intrinsic;


intrinsic JSONIsNull(object:: .) -> BoolElt
{ See if a given object is a JSON null. For now, null is [] in MAGMA }
    return object cmpeq [];
end intrinsic;


function IsEOS(str, index)
    return index gt #str;
end function;


function IsDigit(str)
    code := StringToCode(str) - StringToCode("0");
    return code ge 0 and code lt 10;
end function;


function IsWhitespace(str)
    return str in { " ", "\t", "\n" };
end function;


function SkipWhitespaces(str, index)
    max := #str;
    while not IsEOS(str, index) and IsWhitespace(str[index]) do
        index +:= 1;
    end while;
    return index;
end function;


function ThrowUnexpectedEnd(fname)
    error "Syntax Error: Unexpected end of input in " cat fname;
end function;


function ThrowUnexpectedToken(token, index, fname)
    error "Syntax Error: Unexpected token " cat token cat " at index " cat IntegerToString(index) cat " in "cat fname;
end function;


procedure CheckEOS(str, index, fname)
    if IsEOS(str, index) then
        ThrowUnexpectedEnd(fname);
    end if;
end procedure;

procedure CheckToken(str, index, token, fname)
    character := str[index];
    if character ne token then
        ThrowUnexpectedToken(character, index, fname);
    end if;
end procedure;


procedure CheckTokens(str, index, tokens, fname)
    character := str[index];
    if character notin tokens then
        ThrowUnexpectedToken(character, index, fname);
    end if;
end procedure;

function GetChar(str, index)
    CheckEOS(str, index, "GetChar");
    return str[index];
end function;

function JSONParseObject(str, index)
    //printf "Entering JSONParseObject at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index, "JSONParseObject");
    CheckToken(str, index, "{", "JSONParseObject");
    index +:= 1;

    object := AssociativeArray();
    
    while true do
        hash, value, index := JSONParsePair(str, index);
        object[hash] := value;
        index := SkipWhitespaces(str, index);
        character := GetChar(str, index);
        if character eq "}" then
            break;
        end if;
        if character eq "," then
            index +:= 1;
            continue;
        end if;
        ThrowUnexpectedToken(str[index], index, "JSONParseObject");
    end while;

    //printf "Exiting JSONParseObject at %o\n", index + 1; 
    return object, index + 1;
end function;


function JSONParsePair(str, index)
    //printf "Entering JSONParsePair at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index, "JSONParsePair");
    hash, index := JSONParseString(str, index);
    index := SkipWhitespaces(str, index);
    CheckToken(str, index, ":", "JSONParsePair");
    index +:= 1;
    index := SkipWhitespaces(str, index);
    value, index := JSONParseValue(str, index);
    //printf "Exiting JSONParsePair at %o\n", index;
    return hash, value, index;
end function;


function JSONParseArray(str, index)
    //printf "Entering JSONParseArray at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index, "JSONParseArray 1");
    CheckToken(str, index, "[", "JSONParseArray 1");
    index +:= 1;
    
    array := [* *];
    while true do
        value, index := JSONParseValue(str, index);
        Append(~array, value);
        index := SkipWhitespaces(str, index);
        character := GetChar(str, index);
        if character eq "]" then 
            break;
        end if;
        if character eq "," then
            index +:= 1;
            continue;
        end if;
        ThrowUnexpectedToken(str[index], index, "JSONParseArray 2");
    end while;

    //printf "Exiting JSONParseArray at %o\n", index + 1;
    return array, index + 1;
end function;


function JSONParseValue(str, index)
    //printf "Entering JSONParseValue at %o\n", index;
    index := SkipWhitespaces(str, index);
    character := GetChar(str, index);
    if character eq "\"" then
        value, index := JSONParseString(str, index);
    elif character eq "[" then
        value, index := JSONParseArray(str, index);    
    elif character eq "{" then
        value, index := JSONParseObject(str, index);
    elif IsDigit(character) or character eq "-" then
        value, index := JSONParseNumber(str, index);
    elif not IsEOS(str, index + 3) and str[index .. (index + 3)] eq "true" then
        value := true;
        index +:= 4;
    elif not IsEOS(str, index + 4) and str[index .. (index + 4)] eq "false" then   
        value := false;
        index +:= 5;
    elif not IsEOS(str, index + 3) and str[index .. (index + 3)] eq "null" then
        value := [];        // using [] in lieu of null
        index +:= 4;
    else
        ThrowUnexpectedToken(str[index], index, "JSONParseValue");
    end if;
    
    //printf "Exiting JSONParseValue at %o\n", index;
    return value, index;
end function;


function JSONParseString(str, index)
    //printf "Entering JSONParseString at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index, "JSONParseString");
    CheckToken(str, index, "\"", "JSONParseString 1");
    index +:= 1;
    string := "";
    while GetChar(str, index) ne "\"" do
        character := GetChar(str, index);
        if character ne "\\" then
            string cat:= character;
            index +:= 1;
            continue;
        end if;
        
        index +:= 1;
        character := GetChar(str, index);
        CheckTokens(str, index, { "\"", "\\", "/", "b", "f", "n", "r", "t", "u" }, "JSONParseString 2");
        
        if character eq "u" then
            //error "Syntax Error: Unicode is not yet supported on MAGMA";
            CheckEOS(str, index + 4, "JSONParseString \\u");
            num := StringToInteger(str[(index + 1) .. (index + 4)]);
            str cat:= CodeToString(num);
            index +:= 5;
        else
            case character:
                when "\"": ch := "\"";
                when "\\": ch := "\\";
                when "/": ch := "/";
                when "b": ch := "\b";
                when "f": ch := "\f";
                when "n": ch := "\n";
                when "r": ch := "\r";
                when "t": ch := "\t";
            end case;
            string cat:= ch;
            index +:= 1;
        end if;
        CheckEOS(str, index, "JSONParseString \\other");
    end while;

    //printf "Exiting JSONParseString at %o\n", index + 1;    
    return string, index + 1;
end function;

function JSONParseDigits(str, index)
    //printf "Entering JSONParseDigits at %o\n", index;
    character := GetChar(str, index);
    if not IsDigit(character) then
        ThrowUnexpectedToken(character, index, "JSONParseDigits");
    end if;

    digits := "";
    while not IsEOS(str, index) and IsDigit(str[index]) do
        digits cat:= str[index];
        index +:= 1;
    end while;
    //printf "Exiting JSONParseDigits at %o\n", index;
    return digits, index;
end function;


function JSONParseWhole(str, index)
    //printf "Entering JSONParseWhole at %o\n", index;
    whole := GetChar(str, index) eq "-" select "-" else "";
    index +:= #whole;
    
    if GetChar(str, index) eq "0" then
        digits := "0";
        index +:= 1;
    else
        digits, index := JSONParseDigits(str, index);
    end if;
    whole cat:= digits;

    //printf "Exiting JSONParseWhole at %o\n", index;
    return whole, index;
end function;


function JSONParseFrac(str, index)
    //printf "Entering JSONParseFrac at %o\n", index;

    if IsEOS(str, index) or str[index] ne "." then
        return "", index;
    end if;
    
    digits, index := JSONParseDigits(str, index + 1);

    //printf "Exiting JSONParseFrac at %o\n", index;
    return "." cat digits, index;
end function;


function JSONParseExp(str, index)
    //printf "Entering JSONParseExp at %o\n", index;

    hasexp := not IsEOS(str, index) and (str[index] eq "e" or str[index] eq "E");
    
    if IsEOS(str, index) or (str[index] ne "e" and str[index] ne "E") then
        return "", index;
    end if;
    
    e := str[index];
    index +:= 1;
    
    sign := "";
    character := GetChar(str, index);
    case character:
        when "-": sign := "-";
        when "+": sign := "+";
    end case;
    digits, index := JSONParseDigits(str, index + #sign);

    //printf "Exiting JSONParseExp at %o\n", index;
    return e cat sign cat digits, index;
end function;


function JSONParseNumber(str, index)
    //printf "Entering JSONParseNumber at %o\n", index;
    index := SkipWhitespaces(str, index);
    
    whole, index := JSONParseWhole(str, index);
    frac, index := JSONParseFrac(str, index);
    exponent, index := JSONParseExp(str, index);    

    //printf "Exiting JSONParseNumber at %o\n", index;
    return eval(whole cat frac cat exponent), index;
end function;


function JSONParseTrue(str, index)
    //printf "Entering JSONParseTrue at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index + 3, "JSONParseTrue");
    substr := str[index .. (index + 3)];
    if substr ne "true" then
        ThrowUnexpectedToken(substr, index, "JSONParseTrue");
    end if;
    //printf "Exiting JSONParseTrue at %o\n", index + 4;
    return true, index + 4;
end function;


function JSONParseFalse(str, index)
    //printf "Entering JSONParseFalse at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index + 4, "JSONParseFalse");
    substr := str[index .. (index + 4)];
    if substr ne "false" then
        ThrowUnexpectedToken(substr, index, "JSONParseFalse");
    end if;
    //printf "Exiting JSONParseFalse at %o\n", index + 5;
    return false, index + 5;
end function;


function JSONParseNull(str, index)
    //printf "Entering JSONParseNull at %o\n", index;
    index := SkipWhitespaces(str, index);
    CheckEOS(str, index + 3, "JSONParseNull");
    substr := str[index .. (index + 3)];
    if substr ne "null" then
        ThrowUnexpectedToken(substr, index, "JSONParseNull");
    end if;
    //printf "Exiting JSONParseNull at %o\n", index + 4;
    return [], index + 4;         // no null in MAGMA, so we use []
end function;




intrinsic JSONToString(object::.) -> MonStgElt
{ String representation of a JSON object }
    type := Type(object);
    require type eq Assoc or type eq List or type eq SeqEnum:
        "Output error: Invalid object format";
    
    return JSONPrintInternal(object);
end intrinsic;

function JSONPrintString(string)
    return "\"" cat string cat "\"";
end function;

function JSONPrintNumber(num)
    return Sprintf("%o", num);
end function;

function JSONPrintBool(value)
    return Sprintf("%o", value);
end function;

function JSONPrintNull()
    return "null";
end function;

function JSONPrintPair(hash, value)
    return JSONPrintString(hash) cat ":" cat JSONPrintInternal(value);
end function;

function JSONPrintObject(assoc)
    result := [ "{ " ];
    keys := Setseq(Keys(assoc));
    max := #keys;
    for i in [1 .. (max - 1)] do
        key := keys[i];
        Append(~result, JSONPrintPair(key, assoc[key]));
        Append(~result, ", \n");
    end for;
    if max ne 0 then
        key := keys[max];
        Append(~result, JSONPrintPair(key, assoc[key]));
    end if;
    Append(~result, " }");
    return &cat(result);
end function;

function JSONPrintArray(list)
    result := [ "[ " ];
    max := #list;
    for i in [1 .. (max - 1)] do
        Append(~result, JSONPrintInternal(list[i]));
        Append(~result, ", ");
    end for;
    if max ne 0 then
        Append(~result, JSONPrintInternal(list[max]));
    end if;
    Append(~result, " ]");
    return &cat(result);
end function;

function JSONPrintInternal(object)
    type := Type(object);
    
    if type eq Assoc then
        return JSONPrintObject(object);
    end if;
    
    if type eq List then
        return JSONPrintArray(object);
    end if;
    
    if type eq SeqEnum then
        if JSONIsNull(object) then
            return JSONPrintNull();
        end if;
        return JSONPrintArray(object);
    end if;
        
    if type eq RngIntElt or type eq FldReElt then
        return JSONPrintNumber(object);
    end if;
    
    if type eq MonStgElt then
        return JSONPrintString(object);
    end if;
    
    error "Output error: Invalid object format ";
    return 0;
end function;

