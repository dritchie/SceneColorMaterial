namespace ColorVis
{
    partial class ColorVis
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ColorVis));
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.extractTheme = new System.Windows.Forms.ToolStripButton();
            this.RenderThemes = new System.Windows.Forms.ToolStripButton();
            this.ExtractTemplate = new System.Windows.Forms.ToolStripButton();
            this.RenderWheels = new System.Windows.Forms.ToolStripButton();
            this.toolStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // toolStrip1
            // 
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.extractTheme,
            this.RenderThemes,
            this.ExtractTemplate,
            this.RenderWheels});
            this.toolStrip1.Location = new System.Drawing.Point(0, 0);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(625, 25);
            this.toolStrip1.TabIndex = 1;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // extractTheme
            // 
            this.extractTheme.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.extractTheme.Image = ((System.Drawing.Image)(resources.GetObject("extractTheme.Image")));
            this.extractTheme.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.extractTheme.Name = "extractTheme";
            this.extractTheme.Size = new System.Drawing.Size(83, 22);
            this.extractTheme.Text = "extractTheme";
            this.extractTheme.Click += new System.EventHandler(this.extractTheme_Click);
            // 
            // RenderThemes
            // 
            this.RenderThemes.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.RenderThemes.Image = ((System.Drawing.Image)(resources.GetObject("RenderThemes.Image")));
            this.RenderThemes.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RenderThemes.Name = "RenderThemes";
            this.RenderThemes.Size = new System.Drawing.Size(87, 22);
            this.RenderThemes.Text = "renderThemes";
            this.RenderThemes.Click += new System.EventHandler(this.RenderThemes_Click);
            // 
            // ExtractTemplate
            // 
            this.ExtractTemplate.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ExtractTemplate.Image = ((System.Drawing.Image)(resources.GetObject("ExtractTemplate.Image")));
            this.ExtractTemplate.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ExtractTemplate.Name = "ExtractTemplate";
            this.ExtractTemplate.Size = new System.Drawing.Size(96, 22);
            this.ExtractTemplate.Text = "extractTemplate";
            this.ExtractTemplate.Click += new System.EventHandler(this.ExtractTemplate_Click);
            // 
            // RenderWheels
            // 
            this.RenderWheels.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.RenderWheels.Image = ((System.Drawing.Image)(resources.GetObject("RenderWheels.Image")));
            this.RenderWheels.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RenderWheels.Name = "RenderWheels";
            this.RenderWheels.Size = new System.Drawing.Size(83, 22);
            this.RenderWheels.Text = "renderWheels";
            this.RenderWheels.Click += new System.EventHandler(this.RenderWheels_Click);
            // 
            // ColorVis
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(625, 262);
            this.Controls.Add(this.toolStrip1);
            this.Name = "ColorVis";
            this.Text = "PaletteExtraction";
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton extractTheme;
        private System.Windows.Forms.ToolStripButton RenderThemes;
        private System.Windows.Forms.ToolStripButton ExtractTemplate;
        private System.Windows.Forms.ToolStripButton RenderWheels;
    }
}

