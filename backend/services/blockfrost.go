package services

import (
	"github.com/blockfrost/blockfrost-go"
)

var BFClient blockfrost.APIClient

func InitBlockfrost(apiKey string) {
	BFClient = blockfrost.NewAPIClient(blockfrost.APIClientOptions{
		ProjectID: apiKey,
	})
}
