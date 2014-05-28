#include "file_work.h"

void
transform_data(char* input_file, char* output_file)
{
    FILE* in, *out;
    in = fopen(input_file, "r");
    out = fopen(output_file, "wb+");

    int lines = 0;
    int ch;
    while (EOF != (ch = getc(in)))
        if (ch == '\n')
            ++lines;

    printf("%d  \n", lines);

    fseek(in, 0, SEEK_SET);
    fwrite(&lines, sizeof (lines), 1, out);

    char* line;

    while (readLine(in, &line) != -1) {
        int line_length = strlen(line);
        char* file_line = (char*) malloc(DATA_LENGTH * sizeof (char));
        strncpy(file_line, line, DATA_LENGTH);
        file_line[line_length] = 0;
        fwrite(file_line, DATA_LENGTH, 1, out);
    }
    fclose(out);
    fclose(in);
}

void
read(char* filename)
{
    FILE* f;
    f = fopen(filename, "r");
    int lines;
    fread(&lines, sizeof (lines), 1, f);
    //    printf("%d \n", lines);
    int i;
    for (i = 0; i < lines; ++i) {
        char* line;
        fread(line, DATA_LENGTH * sizeof (char), 1, f);
        printf("read: %d  %s \n", strlen(line), line);

    }
    fclose(f);

}

int
readLine(FILE *file, char** line)
{

    if (file == NULL) {
        printf("Error: file pointer is null.");
        return -1;
    }

    int maximumLineLength = DATA_LENGTH;
    char *lineBuffer = (char *) malloc(sizeof (char) * maximumLineLength);

    if (lineBuffer == NULL) {
        printf("Error allocating memory for line buffer.");
        return -1;
    }

    char ch = getc(file);
    int count = 0;

    while ((ch != '\n') && (ch != EOF)) {
        if (count == maximumLineLength) {
            maximumLineLength += DATA_LENGTH;
            lineBuffer = realloc(lineBuffer, maximumLineLength);
            if (lineBuffer == NULL) {
                printf("Error reallocating space for line buffer.");
                exit(1);
            }
        }
        lineBuffer[count] = ch;
        count++;

        ch = getc(file);
    }

    lineBuffer[count] = '\0';
    *line = (char*) malloc((count + 1) * sizeof (char));
    strncpy(*line, lineBuffer, (count + 1));
    free(lineBuffer);
    if (ch == EOF) return -1;
    return 1;

}

int
main(int argc, char** argv)
{
    transform_data("isolet.data", "mpi_input.bin");
    //    read("mpi_input.bin");

}