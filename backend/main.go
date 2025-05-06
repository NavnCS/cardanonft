package main

import (
	"backend/handlers"
	"backend/services"

	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/cors"
)

func main() {
	app := fiber.New()

	services.InitBlockfrost("YOUR_BLOCKFROST_API_KEY")

	// Apply CORS middleware
	app.Use(cors.New(cors.Config{
		AllowOrigins: "http://localhost:5173",
		AllowMethods: "GET,POST,PUT,DELETE",
		AllowHeaders: "Origin, Content-Type, Accept",
	}))

	// Sample NFT data
	nftData := []map[string]string{
		{"id": "1", "name": "WiraNFT #1", "image": "https://example.com/nft1.png"},
		{"id": "2", "name": "WiraNFT #2", "image": "https://example.com/nft2.png"},
	}

	// Register Fiber routes
	app.Get("/api/nft", func(c *fiber.Ctx) error {
		return c.JSON(nftData)
	})

	app.Get("/api/contract", handlers.GetContractInfo)

	app.Post("/api/mint", handlers.MintNFT)
	app.Post("/api/purchase", handlers.PurchaseNFT)
	app.Get("/api/user-nfts", handlers.GetUserNFTs)

	// Start server
	app.Listen(":8080")
}
