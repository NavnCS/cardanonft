package main

import (
	"log"
	"net/http"

	"backend/handlers"

	"github.com/gorilla/mux"
)

func main() {
	router := mux.NewRouter()

	// Routes
	router.HandleFunc("/api/nft", handlers.GetNFTMetadata).Methods("GET")
	router.HandleFunc("/api/contract", handlers.GetContractInfo).Methods("GET")

	log.Println("Backend running on :8080")
	log.Fatal(http.ListenAndServe(":8080", router))
}
