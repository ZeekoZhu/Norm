using System;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;

namespace Norm.Tests.DbContext
{
    public class Blog
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public bool IsActive { get; set; }
        public List<Category> Categories { get; set; }
    }

    public class BlogPost
    {
        public int Id { get; set; }
        public Blog Blog { get; set; }
        public string Title { get; set; }
        public string Content { get; set; }
        public DateTime PublishedTime { get; set; }
        public bool IsActive { get; set; }
        public List<Tag> Tags { get; set; }
        public Category Category { get; set; }
    }

    public class Tag
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public bool IsActive { get; set; }
    }

    public class Category
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public Blog Blog { get; set; }
        public List<BlogPost> BlogPosts { get; set; }
    }

    public class FooDbContext : Microsoft.EntityFrameworkCore.DbContext
    {
        public DbSet<Category> Categories { get; set; }
        public DbSet<Blog> Blogs { get; set; }
        public DbSet<BlogPost> BlogPosts { get; set; }
        public DbSet<Tag> Tags { get; set; }

        public FooDbContext(DbContextOptionsBuilder builder): base(builder.Options)
        {
            
        }
    }
}