package handlers

import (
	"net/http"
	"os"
)

func GetNFTMetadata(w http.ResponseWriter, r *http.Request) {
	data, err := os.ReadFile("data/nft.json")
	if err != nil {
		http.Error(w, "Failed to read metadata", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(data)
}
