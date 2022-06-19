package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"runtime"
	"strconv"
	"time"

	"github.com/evanw/esbuild/pkg/api"
)

func main() {
	runtime.GOMAXPROCS(1)

	filePath := os.Args[1]
	input, err := ioutil.ReadFile(filePath)
	if err != nil {
		panic(err)
	}

	iterations, err := strconv.ParseUint(os.Args[2], 10, 64)
	if err != nil {
		panic(err)
	}

	start := time.Now()
	var output_len int
	for i := uint64(0); i < iterations; i++ {
		result := api.Transform(string(input), api.TransformOptions{
			LegalComments:     api.LegalCommentsNone,
			Loader:            api.LoaderJS,
			LogLevel:          api.LogLevelError,
			MinifyIdentifiers: true,
			MinifySyntax:      true,
			MinifyWhitespace:  true,
			Sourcemap:         api.SourceMapNone,
			TreeShaking:       api.TreeShakingTrue,
		})

		if len(result.Errors) != 0 {
			panic(result.Errors[0])
		}

		output_len = len(result.Code)
	}
	elapsed_ns := time.Since(start).Nanoseconds()
	fmt.Printf("%d %d\n", output_len, elapsed_ns)
}
