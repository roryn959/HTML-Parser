#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_LINE_LENGTH 50
#define MAX_NUM_LINES 200
#define MAX_NUM_TAGS 200
#define TAG_LENGTH 30

const char tags[14][6] = {"html", "head", "body", "title", "h1", "h2", "h3", "p", "ul", "li", "a", "div", "br", "hr"};

bool validTag(char* s){
    for (int i=0; i<14; i++){
        if (!strcmp(s, tags[i])){
            return true;
        }
    }
    return false;
}

struct tag {
    char* type;
    bool* closing;
};

struct tag* createTag(char* type, bool closing){
    struct tag* newTag = calloc(sizeof(struct tag), 1);

    newTag->type = calloc(sizeof(char), strlen(type)+1);
    strcpy(newTag->type, type);

    newTag->closing = calloc(sizeof(bool), 1);
    *(newTag->closing) = closing;

    return newTag;
}

void cleanup(struct tag** tagStream){
    for (int i=0; i<MAX_NUM_TAGS; i++){
        struct tag* currentTag = *(tagStream+i);
        if (currentTag == NULL){
            free(tagStream);
            return;
        } else {
            free(currentTag);
        }
    }
}

void raiseError(int row, int column, char* message){
    //If row and column are negative, it means the position of the error is unknown
    if (row<0 && column<0) { printf("ERROR\n%s", message); } 
    else { printf("ERROR in row %i, column %i\n%s", row, column, message); }
    exit(-1);
}

FILE* openFile(){
    FILE* file = fopen("file.html", "r");
    if (file == NULL){
        printf("Failed to read file - please make sure the file 'file.html' is placed in '/src'.");
        exit(-1);
    } else {
        return file;
    }
}

char* getFileLine(FILE* filep){
    char* line = calloc(sizeof(char), MAX_LINE_LENGTH);
    if (fgets(line, MAX_LINE_LENGTH, filep) == NULL) { return NULL; }
    else { return line; }
}

char** getFileContents(FILE* filep){
    char** content = calloc(sizeof(char*), MAX_NUM_LINES); //A 2D array of strings, one string for each line
    char* line;

    int i=0;
    while ( (line = getFileLine(filep)) != NULL ){
        char* newLine = line;
        *(content+i) = newLine;
        i++;
    }
    return content;
}

struct tag* processTag(char* line, int row, int column){
    bool closing;
    if (*(line+1) == '/'){
        closing = true;
    } else {
        closing = false;
    }

    //Try to match tag:
    char type[6] = "\0"; //The longest tag in the specification is 'title', which has 5 chars. Include space for EOL char.
    char currentChar;
    int index = 0;

    //If it's a closing tag there is a forward slash, so the text inside the tag starts at index 2. If not, index 1
    for (int i = (closing ? 2 : 1); i<TAG_LENGTH; i++){
        currentChar = *(line+i);
        if (currentChar == '>' || currentChar == ' '){ //If we reach either, we have reached the end of the tag type (excluding attributes)
            break;
        } else {
            type[index] = currentChar;
            index++;
        }
    }

    if (validTag(type)){
        return createTag(type, closing);
    } else {
        char errorMessage[TAG_LENGTH+30] = "Failed to recognise tag ";
        strcat(errorMessage, line);
        raiseError(row, column, errorMessage);
    }
}

struct tag** tokenise(char** content){
    struct tag** tagStream = calloc(sizeof(struct tag*), MAX_NUM_TAGS);
    int tagIndex = 0; //Used to add a tag to the tag stream
    bool inTag = false;
    char* currentTag = calloc(sizeof(char), TAG_LENGTH); //If we are inside a tag, this should track the contents
    int currentIndex = 0; //Used to update current string

    for (int i=0; i<MAX_NUM_LINES; i++){ //For each line
        char* currentLine = *(content+i);
        if (currentLine == NULL){
            break;
        }
        for (int j=0; j<MAX_LINE_LENGTH; j++){ //For each char in line
            char currentChar = *(currentLine+j);
            if (currentChar == '<'){
                if (inTag){ //There should not be a tag definition inside another tag definition
                    raiseError(i+1, j+1, "'<' should not appear inside a tag.");
                    exit(-1);
                } else{
                    *(currentTag) = '<';
                    currentIndex = 1;
                    inTag = true;
                }
            } else if (currentChar == '>'){
                if (!inTag){ //Should never appear unless closing a tag definition
                    raiseError(i+1, j+1, "'>' should not appear outside a tag.");
                } else{
                    *(currentTag+currentIndex) = '>';
                    //Tag correctly closed. Try and match tag, and add to tagStream if valid.
                    struct tag* newTag = processTag(currentTag, i, j); //Pass i and j so error can be raised properly
                    *(tagStream+tagIndex) = newTag;
                    tagIndex++;
                    inTag = false;
                    memset(currentTag, '\0', sizeof currentTag); //Reset currentTag to contain nothing
                }
            } else if (inTag){ //If we are in a tag, track the string inside
                *(currentTag+currentIndex) = currentChar;
                currentIndex++;
            }
        }
    }
    return tagStream;
}

void checkNesting(struct tag** tagStream){
    struct tag** tagStack = calloc(sizeof(struct tag*), MAX_NUM_TAGS);
    int head = 0; //Head always sits at the index of the null character

    for (int i=0; i<MAX_NUM_TAGS; i++){
        struct tag* currentTag = *(tagStream+i);
        //If we've reached the end of the taglist, exit
        if (currentTag == NULL){
            break;
        }
        //<br> and <hr> cannot be improperly nested
        if (!strcmp(currentTag->type, "br") || !strcmp(currentTag->type, "hr")){
            continue;
        }
        if (!*(currentTag->closing)){
            //If opening tag, push onto stack
            tagStack[head] = currentTag;
            head++;
            tagStack[head] = '\0';
        } else{
            //If closing tag, pop from stack. If the types are different, it's incorrectly nested
            if (head==0 || strcmp(tagStack[head-1]->type, currentTag->type) ){ //True if the tags are not the same type
                char errorMessage[TAG_LENGTH+30] = "Tags improperly nested: ";
                strcat(errorMessage, currentTag->type);
                raiseError(-1, -1, errorMessage);
            } else{
                tagStack[head] = '\0';
                head--;
            }
        }
    }
}

void checkHTML(struct tag** tagStream){
    char errorMessage[] = "The entire file should be wrapped in <html> tags.";

    //Check if first tag is <html>
    if ( strcmp( (*(tagStream))->type, "html" ) ){
        raiseError(-1, -1, errorMessage);
    }
    //Find last tag and check if it's html
    for (int i=0; i<MAX_NUM_TAGS; i++){
        if ( *(tagStream+i) == NULL){
            if (strcmp((*(tagStream+i-1))->type, "html")){
                raiseError(-1, -1, errorMessage);
            } else{
                return;
            }
        }
    }
}

void checkHeadAndBody(struct tag** tagStream){
    bool headerFound = false;
    bool bodyFound = false;

    for (int i=0; i<MAX_NUM_TAGS; i++){
        struct tag* currentTag = *(tagStream+i);
        if (currentTag == NULL){
            break;
        } else if (!strcmp(currentTag->type, "head")){
            if (headerFound){
                raiseError(-1, -1, "Only one header section allowed.");
            } else if (bodyFound){
                raiseError(-1, -1, "Header may only come before body.");
            } else if (*currentTag->closing){ //We may assume nesting is correct. In this case, mark as found upon closing tag
                headerFound = true;
            }
        } else if (!strcmp(currentTag->type, "body")){
            if (!headerFound){
                raiseError(-1, -1, "The header must come before the body.");
            } else if (bodyFound){
                raiseError(-1, -1, "Only one body is allowed.");
            } else if (*currentTag->closing){
                bodyFound = true;
            }
        }
    }
    if (!(headerFound && bodyFound)){
        raiseError(-1, -1, "The must be both a single header section and a single body section");
    }
}

void finalCheck(struct tag** tagStream){
    bool inHead = false;
    bool inP = false;

    for (int i=0; i<MAX_NUM_TAGS; i++){
        struct tag* currentTag = *(tagStream+i);
        if (currentTag == NULL){
            break;
        }
        if (!strcmp(currentTag->type, "head")){ //Update whether we're in head
            inHead = !(*currentTag->closing);
        } else if (!strcmp(currentTag->type, "title")){ //Check title tags are inside head
            if (!inHead){
                raiseError(-1, -1, "<title> tags should only be in head sections.");
            }
        } else if (!strcmp(currentTag->type, "p")){
            if (inP){
                if (*currentTag->closing){
                    inP = false;
                } else {
                    raiseError(-1, -1, "<p> tags should not be nested.");
                }
            } else {
                inP = true;
            }
        } else if (!strcmp(currentTag->type, "div")){
            if (inP){
                raiseError(-1, -1, "<div> tags may not be nested inside <p> tags.");
            }
        }
    }
}

void parse(struct tag** tagStream){
    checkNesting(tagStream);
    checkHTML(tagStream); //Checks if the <html></html> lies on the outside of the file
    checkHeadAndBody(tagStream); //Checks if there is a single head followed by a single body
    finalCheck(tagStream); //Checks for remaining rules, such as no nesting <p> tags.
}

int main(){
    FILE* filep = openFile();
    char** fileContents = getFileContents(filep);
    struct tag** tagStream = tokenise(fileContents);
    free(fileContents);
    parse(tagStream);
    printf("Parsing successful.");
    cleanup(tagStream);
    fclose(filep);
}