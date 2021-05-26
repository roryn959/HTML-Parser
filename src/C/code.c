#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 50
#define MAX_NUM_LINES 200
#define MAX_NUM_TAGS 200
#define TAG_LENGTH 30

const char tags[14][6] = {"html", "head", "body", "title", "h1", "h2", "h3", "p", "ul", "li", "a", "div", "br", "hr"};

bool validTag(char* s){
    return true;
}

struct tag {
    char* type;
    bool* closing;
};

struct tag* createTag(char* type, bool closing){
    struct tag *newTag = calloc(sizeof(struct tag), 1);

    newTag->type = calloc(sizeof(char), strlen(type)+1);
    strcpy(newTag->type, type);

    newTag->closing = calloc(sizeof(bool), 1);
    *(newTag->closing) = closing;

    return newTag;
}

void resetLine(char* line){
    for (int i=0; i<MAX_LINE_LENGTH; i++){
        *(line+i) = 0;
    }
}

void raiseError(int row, int column, char* message){
    printf("ERROR at row %i, column %i\n%s", row, column, message);
    exit(-1);
}

FILE* openFile(){
    FILE* file = fopen("file.html", "r");
    
    if (file == NULL){
        printf("Failed to read file - please make sure the file 'file.html' is placed in '/src'.");
        exit(-1);
    } else {
        printf("File opened successfully.\n\n");
        return file;
    }
}

char* getFileLine(FILE* filep){
    char* line = calloc(sizeof(char), MAX_LINE_LENGTH);
    if (fgets(line, MAX_LINE_LENGTH, filep) == NULL){
        return NULL;
    } else{
        return line;
    }
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

struct tag* processTag(char* line){
    printf("Processing %s", line);

    bool closing;
    
    if (*(line+1) == '/'){
        closing = true;
    } else {
        closing = false;
    }

    //Try to match tag:

    char type[6] = {0, 0, 0, 0, 0, '\0'}; //The longest tag in the specification is 'title', which has 5 chars. Include space for EOL char.
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
        printf(closing ? "close\n" : "open\n");
        return createTag(type, closing);
    } else {
        return NULL;
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
                    struct tag* newTag = processTag(currentTag);

                    if (newTag == NULL){
                        raiseError(i+1, j+1, strcat("Failed to recognise tag: ", currentTag));
                    }

                    *(tagStream+tagIndex) = newTag;
                    tagIndex++;

                    inTag = false;
                    resetLine(currentTag); //Reset currentTag to contain nothing
                }

            } else if (inTag){ //If we are in a tag, track the string inside
                *(currentTag+currentIndex) = currentChar;
                currentIndex++;
            }
        }
    }
    return tagStream;
}

int main(){
    printf("Opening file...\n");
    FILE* filep = openFile();

    printf("Reading file contents...\n");
    char** fileContents = getFileContents(filep);

    printf("Lexing...\n");
    struct tag** tagStream = tokenise(fileContents);

    for (int i=0; i<MAX_NUM_TAGS; i++){
        struct tag* newTag = *(tagStream+i);

        if (newTag == NULL){
            break;
        }

        struct tag actualTag = *(newTag);

        printf("Type: %s ", actualTag.type);
        printf(actualTag.closing ? "closing\n" : "opening\n");
    }

    printf("Parsing...");

    fclose(filep);
}