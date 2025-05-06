package handlers

import (
	"github.com/gofiber/fiber/v2"
)

func GetUserNFTs(c *fiber.Ctx) error {
	wallet := c.Query("wallet")
	if wallet == "" {
		return c.Status(fiber.StatusBadRequest).JSON(fiber.Map{
			"error": "Wallet address required",
		})
	}

	// 🔁 MOCK RESPONSE – Replace with real NFT fetching logic later
	nfts := []map[string]string{
		{
			"id":    "1",
			"name":  "WiraNFT #1",
			"image": "https://example.com/nft1.png",
		},
		{
			"id":    "2",
			"name":  "WiraNFT #2",
			"image": "https://example.com/nft2.png",
		},
	}

	return c.JSON(nfts)
}
