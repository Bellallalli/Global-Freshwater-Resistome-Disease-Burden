import pandas as pd
import numpy as np
import os
import warnings
import matplotlib.pyplot as plt
from sklearn.tree import DecisionTreeRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.linear_model import BayesianRidge, Ridge, Lasso
from lightgbm import LGBMRegressor
from xgboost import XGBRegressor
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.model_selection import cross_val_score, KFold

# 忽略警告
warnings.filterwarnings('ignore')

# ==========================================
# 1. 路径设置与数据读取
# ==========================================
train_path = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\02.dataset\02.4Economy-RM-Trainset.xlsx"
test_path = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\02.dataset\02.4Economy-RM-Testset.xlsx"
feature_path = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\03.ElasticNet\RM.xlsx"
output_dir = r"F:\Global Samples\Model Data\2026-01-12 Economy-4dataset\7Train-3Test-Final\04.Best Feature"

os.makedirs(output_dir, exist_ok=True)

# 读取数据
df_train = pd.read_excel(train_path, index_col=0)
df_test = pd.read_excel(test_path, index_col=0)
df_train.columns = df_train.columns.str.strip()
df_test.columns = df_test.columns.str.strip()

# 读取特征并清洗
selected_features_df = pd.read_excel(feature_path)
all_ordered_features = selected_features_df['Feature'].str.strip().tolist()

target_col = df_train.columns[-1]
y_train = df_train[target_col].values
y_test = df_test[target_col].values

# 定义模型
models = {
    "DecisionTree": DecisionTreeRegressor(random_state=42),
    "RandomForest": RandomForestRegressor(n_estimators=100, random_state=42),
    "SVR": SVR(C=1.0, epsilon=0.1),
    "KNN": KNeighborsRegressor(n_neighbors=3),
    "BayesianRidge": BayesianRidge(),
    "Ridge": Ridge(alpha=1.0),
    "LASSO": Lasso(alpha=0.1),
    "LightGBM": LGBMRegressor(n_estimators=100, verbose=-1, random_state=42),
    "XGBoost": XGBRegressor(n_estimators=100, verbosity=0, random_state=42)
}

# ==========================================
# 2. 渐进式特征筛选 (15 to 50)
# ==========================================
iteration_results = []
kf = KFold(n_splits=5, shuffle=True, random_state=42)

print("正在执行 15-50 个特征的渐进式筛选...")

for k in range(15, 51):
    current_features = all_ordered_features[:k]
    X_train_sub = df_train[current_features].values

    best_k_r2 = -np.inf
    best_k_model = ""

    for name, model in models.items():
        try:
            cv_scores = cross_val_score(model, X_train_sub, y_train, cv=kf, scoring='r2')
            avg_r2 = np.mean(cv_scores)
            if avg_r2 > best_k_r2:
                best_k_r2 = avg_r2
                best_k_model = name
        except:
            continue

    iteration_results.append({
        'Feature_Count': k,
        'Best_Method': best_k_model,
        'CV_R2': best_k_r2
    })
    print(f"特征数 {k}: 最佳方法 {best_k_model}, CV R² {best_k_r2:.4f}")

# 找到全局最佳组合
iteration_df = pd.DataFrame(iteration_results)
best_overall = iteration_df.loc[iteration_df['CV_R2'].idxmax()]
opt_k = int(best_overall['Feature_Count'])
opt_features = all_ordered_features[:opt_k]

# ==========================================
# 3. 最佳组合的九模型测试与绘图
# ==========================================
print(f"\n最佳组合已找到：特征数 = {opt_k}")
X_final_train = df_train[opt_features].values
X_final_test = df_test[opt_features].values

fig, axes = plt.subplots(3, 3, figsize=(20, 20))
axes = axes.flatten()

final_metrics = []

for i, (name, model) in enumerate(models.items()):
    model.fit(X_final_train, y_train)
    y_pred = model.predict(X_final_test)

    r2 = r2_score(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)

    final_metrics.append({'Model': name, 'Test_R2': r2, 'Test_MSE': mse})

    # 绘图逻辑 (对数刻度显示)
    ax = axes[i]
    ax.scatter(y_test, y_pred, alpha=0.6, s=50, color='#5DADE2')
    mn, mx = min(y_test.min(), y_pred.min()), max(y_test.max(), y_pred.max())
    ax.plot([mn, mx], [mn, mx], 'r--', lw=2)

    tick_values = np.arange(np.floor(mn), np.ceil(mx) + 0.5, 0.5)
    tick_labels = [f'$10^{{{t:.1f}}}$' if t % 1 != 0 else f'$10^{{{int(t)}}}$' for t in tick_values]
    ax.set_xticks(tick_values);
    ax.set_xticklabels(tick_labels, fontsize=11)
    ax.set_yticks(tick_values);
    ax.set_yticklabels(tick_labels, fontsize=11)

    ax.set_title(f"{name}", fontsize=16, fontweight='bold', pad=25)
    ax.text(0.5, 1.03, f"R²: {r2:.3f} | MSE: {mse:.3e}", fontsize=13, ha='center', va='bottom', transform=ax.transAxes)
    ax.set_xlabel("y_test", fontsize=12);
    ax.set_ylabel("y_predict", fontsize=12)
    ax.grid(True, linestyle=':', alpha=0.6)
    ax.set_xlim(mn, mx);
    ax.set_ylim(mn, mx)

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.suptitle(f"Performance Comparison on Test Set (Top {opt_k} Features)", fontsize=22, fontweight='bold')
plt.show()

# ==========================================
# 4. 打印最终结果
# ==========================================
print("\n" + "!" * 40)
print(f"最终最佳特征组合 (共 {opt_k} 个):")
print(opt_features)
print("-" * 40)
print("各模型在测试集上的最终表现 (有序):")
final_metrics_df = pd.DataFrame(final_metrics).sort_values(by='Test_R2', ascending=False)
print(final_metrics_df.to_string(index=False))
print("!" * 40)

# 保存
with pd.ExcelWriter(os.path.join(output_dir, 'Optimal_Feature_Analysis.xlsx')) as writer:
    iteration_df.to_excel(writer, sheet_name='Search_Process', index=False)
    final_metrics_df.to_excel(writer, sheet_name='Final_Test_Performance', index=False)
    pd.DataFrame({'Best_Features': opt_features}).to_excel(writer, sheet_name='Feature_List', index=False)