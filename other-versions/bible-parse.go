package main

import (
	"bufio"
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

// Parse string to extract verse
func parseVerseLine(line string) (*BibleVerse, error) {
	parts := strings.Split(line, "\t")
	if len(parts) != 5 {
		return nil, fmt.Errorf("invalid verse line format: %s", line)
	}

	chapter, err := strconv.Atoi(parts[2])
	if err != nil {
		return nil, fmt.Errorf("invalid chapter number: %s", parts[2])
	}

	verse, err := strconv.Atoi(parts[3])
	if err != nil {
		return nil, fmt.Errorf("invalid verse number: %s", parts[3])
	}

	return &BibleVerse{
		Abbrev:  strings.ToLower(parts[1]),
		Chapter: chapter,
		Verse:   verse,
		Text:    parts[4],
	}, nil
}

var kjvVerses []BibleVerse

func loadVerses(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return fmt.Errorf("error opening file: %v", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		verse, err := parseVerseLine(scanner.Text())
		if err == nil {
			kjvVerses = append(kjvVerses, *verse)
		}
	}

	return scanner.Err()
}

func displayVerse(verse BibleVerse) {
	fmt.Println(verse.Text)
}

// ;Filter verses by book
func hasPrefix(prefixes []string, s string) bool {
	s = strings.ToLower(s)
	for _, prefix := range prefixes {
		prefix = strings.ToLower(prefix)
		if len(prefix) <= len(s) && strings.HasPrefix(s, prefix) {
			return true
		}
	}
	return false
}

// Process query, display matching verses
func processQuery(query string) {
	parts := strings.Fields(query)
	if len(parts) == 0 {
		fmt.Println("Usage: kjv <book> [chapter:verse]")
		os.Exit(1)
	}

	// Some books have spaces: "1 Kings"
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
		if hasPrefix([]string{"ge", "gen", "gene"}, verse.Abbrev) { // Add more abbreviations as needed
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

	// Handle chapter:verse format
	parts = strings.Split(chapterVerse, ":")
	chapter, err := strconv.Atoi(parts[0])
	if err != nil {
		fmt.Printf("Invalid chapter number: %s\n", parts[0])
		os.Exit(1)
	}

	// Just chapter: `genesis 1`
	chapterVerses := make([]BibleVerse, 0)
	for _, verse := range matchingVerses {
		if verse.Chapter == chapter {
			chapterVerses = append(chapterVerses, verse)
		}
	}

	if len(parts) == 2 {
		//; Chapter and verse: `genesis 1:1`
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
		// Whole book queries: `genesis`
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
	if err := loadVerses("kjv.tsv"); err != nil {
		fmt.Printf("Error loading verses: %v\n", err)
		os.Exit(1)
	}

	if len(os.Args) < 2 {
		fmt.Println("Usage: kjv <book> [chapter:verse]")
		os.Exit(1)
	}

	query := strings.Join(os.Args[1:], " ")
	processQuery(query)
}
