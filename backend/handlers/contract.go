package handlers

import (
	"encoding/json"
	"net/http"
)

func GetContractInfo(w http.ResponseWriter, r *http.Request) {
	info := map[string]string{
		"scriptAddress": "addr_test1xyz...abc", // replace with real one later
		"policyId":      "123abc...",           // fake for now
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(info)
}
