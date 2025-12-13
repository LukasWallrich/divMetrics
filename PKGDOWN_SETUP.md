# Setting Up pkgdown Website

This package now has pkgdown configured to automatically build and
deploy documentation to GitHub Pages.

## What’s Been Set Up

1.  \*\*\_pkgdown.yml\*\* - Configuration file for the website structure
    and appearance
2.  **.github/workflows/pkgdown.yaml** - GitHub Actions workflow to
    build and deploy the site
3.  **DESCRIPTION** - Updated with URL and BugReports fields
4.  **NEWS.md** - Changelog for tracking package updates

## Enabling GitHub Pages

To activate the website, you need to enable GitHub Pages in your
repository:

1.  **Push these changes to GitHub:**

    ``` bash
    git add .
    git commit -m "Add pkgdown website configuration"
    git push
    ```

2.  **Enable GitHub Pages:**

    - Go to your repository on GitHub:
      <https://github.com/LukasWallrich/divMetrics>
    - Click on **Settings** (top menu)
    - In the left sidebar, click **Pages**
    - Under **Source**, select:
      - **Branch:** `gh-pages`
      - **Folder:** `/ (root)`
    - Click **Save**

3.  **Wait for the workflow to run:**

    - Go to the **Actions** tab in your repository
    - You should see a “pkgdown” workflow running
    - Once it completes (green checkmark), your site will be live

4.  **View your site:**

    - The site will be available at:
      <https://lukaswallrich.github.io/divMetrics/>
    - It may take a few minutes after the first deployment

## Automatic Updates

The website will automatically rebuild and redeploy when you: - Push to
the `main` or `master` branch - Create a pull request (for preview) -
Publish a release - Manually trigger the workflow (Actions tab → pkgdown
→ Run workflow)

## Customizing the Website

You can customize the website by editing `_pkgdown.yml`:

- **Theme:** Currently using Bootstrap 5 with Cosmo theme
- **Navigation:** Organized into Reference, Articles, and News sections
- **Function grouping:** Functions are organized by type (Separation,
  Variety, Hybrid)

## Testing Locally

To build and preview the site locally (requires Pandoc):

``` r
# Install pkgdown if needed
install.packages("pkgdown")

# Build the site
pkgdown::build_site()

# Preview in browser
pkgdown::preview_site()
```

Note: The GitHub Action will build the site even if you can’t build it
locally.

## Troubleshooting

If the website doesn’t appear:

1.  Check the Actions tab for build errors
2.  Ensure GitHub Pages is enabled in Settings → Pages
3.  Verify the `gh-pages` branch exists after the workflow runs
4.  Check that the workflow has write permissions (Settings → Actions →
    General → Workflow permissions → “Read and write permissions”)

## Additional Resources

- [pkgdown documentation](https://pkgdown.r-lib.org/)
- [GitHub Pages documentation](https://docs.github.com/en/pages)
- [GitHub Actions documentation](https://docs.github.com/en/actions)
