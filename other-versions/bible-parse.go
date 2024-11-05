package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type BibleVerse struct {
	Abbrev  string
	Chapter int
	Verse   int
	Text    string
}

var kjvVerses []BibleVerse

// Allow ge, gen, gene etc. based on the abbreviated ge
func matchBook(query, abbrev string) bool {
	query = strings.ToLower(query)
	return strings.HasPrefix(abbrev, query) || strings.HasPrefix(query, abbrev)
}

func displayVerse(verse BibleVerse) {
	fmt.Printf("%s\n", verse.Text)
}

func processQuery(query string) {
	parts := strings.Fields(query)
	if len(parts) == 0 {
		fmt.Println("Usage: kjv <book> [chapter:verse]")
		os.Exit(1)
	}

	var bookQuery string
	var chapterVerse string
	if len(parts) > 1 {
		lastPart := parts[len(parts)-1]
		if strings.Contains(lastPart, ":") || strings.ContainsAny(lastPart, "0123456789") {
			bookQuery = strings.ToLower(strings.Join(parts[:len(parts)-1], " "))
			chapterVerse = lastPart
		} else {
			bookQuery = strings.ToLower(strings.Join(parts, " "))
		}
	} else {
		bookQuery = strings.ToLower(parts[0])
	}

	// Filter verses by book
	matchingVerses := make([]BibleVerse, 0)
	for _, verse := range kjvVerses {
		if matchBook(bookQuery, verse.Abbrev) {
			matchingVerses = append(matchingVerses, verse)
		}
	}

	if len(matchingVerses) == 0 {
		fmt.Printf("No match found for: %s\n", bookQuery)
		os.Exit(1)
	}

	// Whole book queries: `genesis`
	if chapterVerse == "" {
		for _, verse := range matchingVerses {
			displayVerse(verse)
		}
		return
	}

	// Chapter and verse: `genesis 1:1`
	parts = strings.Split(chapterVerse, ":")
	chapter, err := strconv.Atoi(parts[0])
	if err != nil {
		fmt.Printf("Invalid chapter number: %s\n", parts[0])
		os.Exit(1)
	}

	chapterVerses := make([]BibleVerse, 0)
	for _, verse := range matchingVerses {
		if verse.Chapter == chapter {
			chapterVerses = append(chapterVerses, verse)
		}
	}

	if len(parts) == 2 {
		// Chapter and verse: `genesis 1:1`
		verseNum, err := strconv.Atoi(parts[1])
		if err != nil {
			fmt.Printf("Invalid verse number: %s\n", parts[1])
			os.Exit(1)
		}

		found := false
		for _, verse := range chapterVerses {
			if verse.Verse == verseNum {
				displayVerse(verse)
				found = true
				break
			}
		}
		if !found {
			fmt.Printf("No verse found for: %s %s\n", bookQuery, chapterVerse)
		}
	} else {
		// Just chapter: `genesis 1`
		if len(chapterVerses) == 0 {
			fmt.Printf("No chapter found for: %s %s\n", bookQuery, chapterVerse)
		} else {
			for _, verse := range chapterVerses {
				displayVerse(verse)
			}
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: kjv <book> [chapter:verse]")
		os.Exit(1)
	}

	query := strings.Join(os.Args[1:], " ")
	processQuery(query)
}
