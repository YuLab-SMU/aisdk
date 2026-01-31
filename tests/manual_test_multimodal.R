
# Manual Verification for Image Support

library(aisdk)
library(base64enc)

# --- Test 1: URL Image ---
message("\n--- Test 1: URL Image ---")
url_img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/Gfp-wisconsin-madison-the-nature-boardwalk.jpg/2560px-Gfp-wisconsin-madison-the-nature-boardwalk.jpg"

content_list <- list(
  content_text("What is in this image? Describe it briefly."),
  content_image(url_img)
)

print(str(content_list))

# --- Test 2: Local Image (Mock) ---
# Create a dummy image file for testing
fs::file_create("test_image.png")
png("test_image.png")
plot(1, 1, main = "Test Image")
dev.off()

message("\n--- Test 2: Local Image ---")
local_content <- tryCatch({
  content_image("test_image.png")
}, error = function(e) {
  message("Error: ", e$message)
  NULL
})


if (!is.null(local_content)) {
  message("Successfully created local image content.")
  message("Type: ", local_content$type)
  message("URL start: ", substr(local_content$image_url$url, 1, 30), "...")
}

# Cleanup
if (file.exists("test_image.png")) unlink("test_image.png")
