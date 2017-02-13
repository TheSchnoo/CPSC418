// hw3.c: merge sort for homework 3.
//   You need to complete main.  See the comments in the main(argc, argv)
//   function.
//
//   To compile this file:
//     gcc -std=c99 hw3.c -o hw3
//   You should execute the list and array sorting routines and
//   report their run-times.  For example:
/*
    struct rusage r0, r1;
    getrusage(RUSAGE_SELF,  &r0); // record starting time 
    measure_the_execution_time_of_this_function(...);
    getrusage(RUSAGE_SELF,  &r1);  // record the ending time 
    t_elapsed =   (r1.ru_utime.tv_sec - r0.ru_utime.tv_sec)
		+ 1e-6*(r1.ru_utime.tv_usec - r0.ru_utime.tv_usec);
*/
//  The functions that are already written are:
//    List *merge_sort_list(List *data): sort the list data into ascending order
//    List *list_rand(int n): create a list of n random integers
//    void merge_int(int *data, int n): sort the array data into ascending order.  data has n elements
//    void array_rand(int n): create an array of n random integers.
//  These are the main ones.  There are others that are called by these
//  plus a couple of functions that I found helpful while writing the code.
//  The other functions are documented where they appear in the code.

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/resource.h>

// constants for describing the test to run
#define LIST 1
#define ARRAY 2
#define RANDOM 10
#define ASCENDING 11
#define DEFAULT_N 1000000
#define DEFAULT_NTRIAL 1
#define BIG 100000000

typedef struct List {
  struct List *next;
  int value;
} List;

// merge(x, y)
//   x and y are sorted list
//   return the sorted, merged list
List *merge(List *x, List *y) {
    List *first, *last;
    if(x->value <= y->value) {
	first = x;
	x = x->next;
    } else {
	first = y;
	y = y->next;
    }
    last = first;
    while((x != NULL) && (y != NULL)) {
	if(x->value <= y->value) {
	    last = last->next = x;
	    x = x->next;
	} else {
	    last = last->next = y;
	    y = y->next;
	}
    }
    if(x == NULL) last->next = y;
    else last->next = x;
    return(first);
}

// a is a list, n is the length of a
//   Return a sorted into ascending order.
List *merge_sort_2(List *data, int n) {
    // divide List *a into two, roughly equal length pieces
    List *first_half = data, *x = data;
    if(n <= 1) return(data);
    for(int i = 1; i < n/2; i++) {
	x = x->next;
    }
    List *last_half = x->next;
    x->next = NULL;
    List *first_sorted = merge_sort_2(first_half, n/2);
    List *last_sorted = merge_sort_2(last_half, n - (n/2));
    List *sorted = merge(first_sorted, last_sorted);
    return(sorted);
}

// data is a list.
//   Return a sorted into ascending order.
//   We re-arrange the pointers of data; so the original list is lost.
List *merge_sort_list(List *data) {
    int n = 0;
    // figure out the length of List *a
    for(List *x = data; x != NULL; x = x->next)
	n++;
    // now sort it
    return(merge_sort_2(data, n));
}

// create a list of n random integers
List *list_rand(int n, int init_data) {
    // create a random list of n elements
    List *a = NULL, *b;
    for(int i = 0; i < n; i++) {
	b = (List *)malloc(sizeof(List));
	b->value = (init_data == RANDOM) ? random() : i;
	b->next = a;
	a = b;
    }
    return(a);
}


// print_array: print the elements of an array of integers,
//   four values per line.  
//   Note: this function isn't used in this template or solution,
//   but I found it handy when checking the code; so, I've left
//   it for you.
void print_array(int *data, int n) {
    for(int i = 0; i < n; i += 4) {
        for(int j = 0; (j < 4) && (i+j) < n; j++)
	  printf("%12d", data[i+j]);
	printf("\n");
    }
}

// min(a,b) 
//   When printing arrays, I found it helpful to just print the first
//   40 or so elements if the array is huge.  Calling
//     print_array(array_to_print, min(array_length, 40));
//   does that.
int min(a, b) {
  return(a <= b ? a : b);
}


// Now, I'll implement merge-sort to work with an array of integers.

// merge_int: merge two, contiguous arrays of integers into one sorted array.
//   src[0 .. n/2 - 1] is the first sorted arrray, and src[n/2 .. n-1] is the second.
//   the result is stored in dst[0..n].
// Perhaps I went a bit overboard to avoid extra loads and array copies.
// I wanted the version to be fairly efficient to make the effects of
// branch mispredictions and/or cache misses show up clearly in the run
// times.
void merge_int(int *src, int *dst, int n) {
    int n2 = n >> 1;
    int *src1 = src,  *src2 = src+n2;
    int *top1 = src2, *top2 = src+n;
    int x1 = *src1, x2 = *src2;

    while((src1 < top1) || (src2 < top2)) {
	if(x1 < x2) {
	    // take the element from src1
	    *(dst++) = x1;
	    src1++;
	    if(src1 == top1)
	        // We've finished processing src1.  We could now just copy
	        // what's left in src2 -- I tried it, it's about 12% faster.
		// But, reasoning about program execution is easier we stay
		// in the original loop.  I'll set x1 to be greater than
		// the greatest element element in the src2 array.
	        x1 = top2[-1]+1;
	    else x1 = *src1;
	} else {
	    *(dst++) = x2;
	    src2++;
	    if(src2 == top2)
	      // see the coment above for the src1 == top1 case
	      x2 = top1[-1]+1;
	    else x2 = *src2;
	}
    }
}

// merge_sort_help -- the main part of merge_sort for arrays of integers
//   int *data  -- the array to sort
//   int n      -- the number of elements in data
//   int depth  -- counts the depth of the recursive calls.
//                   For the top-level call, depth=0.
//                   If depth is even, the sorted result will be stored in data.
//                   If depth is odd, the sorted result will be stored in tmp.
//                   This avoids doing extra copies.
//   int *tmp   -- an array of n ints for intermediate storage.
void merge_sort_help(int *data, int n, int depth, int *tmp) {
    if(n <= 1) {
        if(depth & 1) *tmp = *data;
	return;
    }
    int n2 = n >> 1;
    merge_sort_help(data, n2, depth+1, tmp);
    merge_sort_help(data+n2, n-n2, depth+1, tmp+n2);
    if(depth & 1)
	  merge_int(data, tmp, n);
    else
	  merge_int(tmp, data, n);
}

// merge_sort_array, the top-level wrapper for merge-sort
void merge_sort_array(int *data, int n) {
  int *tmp = (int *)malloc(n*sizeof(int));
  merge_sort_help(data, n, 0, tmp);
  free(tmp);
}
  

// array_rand(n) -- return an array or n random integers
int *array_rand(int n, int init_data) {
  int *rdata = (int *)malloc(n*sizeof(int));
  for(int i = 0; i < n; i++)
    rdata[i] = (init_data == RANDOM) ? random() : i;
  return(rdata);
}


struct hw3_args{
  int data_struct,
      init_data,
      n,
      n_trial;
};

void usage(char *fmt, char *etc) {
    fprintf(stderr, "usage: hw3 init_data data_struct [n [n_trial]]\n");
    fprintf(stderr, "  where: init_data is \"random\" or \"ascending\",\n");
    fprintf(stderr, "         data_struct is \"list\" or \"array\",\n");
    fprintf(stderr, "         n is the number of elements to sort %s %d, and\n",
    	           "-- optional, default = ", DEFAULT_N);
    fprintf(stderr, "         n_trial is the number of times to run the sorting function %s %d\n",
    	           "-- optional, default = ", DEFAULT_NTRIAL);
    if(fmt != NULL) {
        fprintf(stderr, fmt, etc);
        fprintf(stderr, "\n");
    }
    exit(-1);
}

void process_args(int argc, char **argv, struct hw3_args *args) {
    if((argc < 3) || (argc > 5)) usage(NULL, NULL);
    if(strcmp(argv[1], "random") == 0) args->init_data = RANDOM;
    else if(strcmp(argv[1], "ascending") == 0) args->init_data = ASCENDING;
    else usage("unrecognized value for init_data:  ", argv[1]);
    if(strcmp(argv[2], "list") == 0) args->data_struct = LIST;
    else if(strcmp(argv[2], "array") == 0) args->data_struct = ARRAY;
    else usage("unrecognized value for data_struct:  ", argv[2]);
    if(argc >= 4) {
        args->n = atoi(argv[3]);
	if(args->n <= 0) usage("invalid value for n:  ", argv[3]);
    } else args->n = DEFAULT_N;
    if(argc >= 5) {
        args->n_trial = atoi(argv[4]);
	if(args->n_trial <= 0) usage("invalid value for n_trial:  ", argv[4]);
    } else args->n_trial = DEFAULT_NTRIAL;
}

// main(argc, argv)
//   This is mostly a code reading problem.  There are two comments that 
//   include the word "WRITE" -- that's where you need to write a few lines
//   of code to complete the problem.
//
//   Why did we do such a crazy thing?
//   Often, software development involves more code reading than code writing.
//   It is also our plan to have you read this code, understand how we can
//   taking timing measurements and report the results; so you'll be ready
//   to do that with the CUDA programming after the midterm.
int main(int argc, char **argv) {
    struct List *list_unsort, *list_sort;
    int *array;
    struct hw3_args args;
    struct rusage r0, r1;
    double t_elapsed, t_avg;

    process_args(argc, argv, &args);

    // create a random array or list according to args.data_struct
    if(args.data_struct == LIST) list_unsort = list_rand(args.n, args.init_data);
    else {
      array = array_rand(args.n, args.init_data);
    }
    // Note: the homework problem calls for making multiple runs, but only
    // when the array structure is used (to make your task easier).   OTOH,
    // random number generation is slow; so, you don't want to call
    // array_rand inside the loop with the timing measurement.
    // Mark's solution allocates a second array, and copies the random one
    // into the new array with each iteration.  That means we'll accept the
    // overhead of an array copy in addition to the sort inside the loop
    // body.
    // Ian proposed allocating enough data to use a different array with
    // each trial.  Ian's approach will will give more accurate results,
    // but you have to be careful to make sure that n*n_trial isn't too big
    // (and will cause malloc to fail).
    // Either approach is acceptable for solving the homework problem.
    //
    // tl;dr;
    // WRITE the code to provide random data on each run of n_trial runs.

    // now, we'll check the time, run the trials, and check the time again.
    getrusage(RUSAGE_SELF,  &r0); /* record starting time */
    for(int i = 0; i < args.n_trial; i++) {

        // WRITE the loop-body to call merge_sort_list or merge_sort_array
        if(args.data_struct == LIST) list_sort = merge_sort_list(list_unsort);
        else {
            int *array2 = malloc(args.n * sizeof(int));     // New array for random values
            memcpy(array2, array, args.n * sizeof(int));    // Copy array into array2
            merge_sort_array(array2, args.n);               // Merge sort the array
            free(array2);
        }
    }
    getrusage(RUSAGE_SELF,  &r1);  /* record the ending time */
    t_elapsed =   (r1.ru_utime.tv_sec - r0.ru_utime.tv_sec)
		+ 1e-6*(r1.ru_utime.tv_usec - r0.ru_utime.tv_usec);
    t_avg = t_elapsed / args.n_trial;

    // print the result.
    printf("%s %s n=%d n_trial=%d t_avg=%0.3e\n",
      (args.init_data == ASCENDING) ? "ascending" : "random",
      (args.data_struct == LIST) ? "list" : "array",
      args.n, args.n_trial, t_avg);
    exit(0);
}
