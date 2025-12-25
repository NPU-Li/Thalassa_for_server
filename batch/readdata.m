%% 参数设置
basePath = '/public/home/guojg/thalassa_dir/output_data/test1/C001';
nSats    = 6;  % 卫星总数

%% Step 1: 读取 SID 1（参考轨迹）
fprintf('正在读取 SID 1 数据...\n');
refFolder = sprintf('S%010d', 1);
refPath   = fullfile(basePath, refFolder, 'outcart.dat');

if ~isfile(refPath)
    error('找不到文件: %s', refPath);
end

fid = fopen(refPath, 'r');
rawData = textscan(fid, '%f%f%f%f%f%f%f', ...
                   'Delimiter', ',', ...
                   'CommentStyle', '#');
fclose(fid);

t_ref   = rawData{1};                              % 时间序列 (MJD, 单位天)
pos_ref = [rawData{2}, rawData{3}, rawData{4}];    % 位置 (km)

nTimes  = length(t_ref);
%% 打印时间步长信息
dt_days    = mean(diff(t_ref));
dt_hours   = dt_days * 24;
dt_minutes = dt_days * 24 * 60;
fprintf('时间步长: %.12f 天 (%.6f 小时, %.3f 分钟)\n', ...
         dt_days, dt_hours, dt_minutes);

fprintf('SID 1 数据读取完成，共 %d 行。\n', nTimes);

%% Step 2: 循环处理 SID 2 ~ nSats，找最小距离
minDist = zeros(1, nSats-1);
minTime = zeros(1, nSats-1);

for sid = 2:nSats
    fprintf('\n正在读取并对比 SID %d...\n', sid);
    sidFolder = sprintf('S%010d', sid);
    filePath  = fullfile(basePath, sidFolder, 'outcart.dat');

    if ~isfile(filePath)
        error('找不到文件: %s', filePath);
    end

    % 分批读取
    fid = fopen(filePath, 'r');
    fileInfo = dir(filePath);
    fileSize = fileInfo.bytes;
    bytesRead = 0;

    minDistSid = inf;
    minTimeSid = NaN;
    lineCount  = 0;

    wb = waitbar(0, sprintf('SID %d: 正在读取和对比...', sid));

    while ~feof(fid)
        blockData = textscan(fid, '%f%f%f%f%f%f%f', ...
                              500000, ... % 每批50万行
                              'Delimiter', ',', ...
                              'CommentStyle', '#');

        if isempty(blockData{1})
            break;
        end

        bytesRead = ftell(fid);

        t_cur   = blockData{1};
        pos_cur = [blockData{2}, blockData{3}, blockData{4}];

        idx_start = lineCount + 1;
        idx_end   = lineCount + size(pos_cur,1);

        diffVec = pos_cur - pos_ref(idx_start:idx_end, :);
        distVec = sqrt(sum(diffVec.^2, 2));

        [batchMin, batchIdx] = min(distVec);
        if batchMin < minDistSid
            minDistSid = batchMin;
            minTimeSid = t_ref(idx_start + batchIdx - 1);
        end

        lineCount = idx_end;
        waitbar(bytesRead / fileSize, wb);
    end

    fclose(fid);
    close(wb);

    minDist(sid-1) = minDistSid;
    minTime(sid-1) = minTimeSid;

    fprintf('SID %d 处理完成: 最近距离 = %.6f km, 时间 MJD = %.9f', ...
            sid, minDistSid, minTimeSid);
end

%% Step 3: 输出总结果
fprintf('\n==== 相对于 SID 1 的最近距离汇总 ====\n');
for sid = 2:nSats
    fprintf('SID %d: 最近距离 = %.6f km, 时间 MJD = %.9f \n', ...
            sid, minDist(sid-1), minTime(sid-1));
end

% %% Step 4: 绘制距离变化曲线并保存数据
% % 以 SID2 最近距离的时间为基准
% T_base = minTime(1);  % SID2 最近距离发生时间
% x_window_min = 5/60;    % 窗口半宽，单位分钟
% x_window_days = x_window_min / 1440 ;  % 转为天
% 
% % 时间筛选
% time_mask = (t_ref >= T_base - x_window_days) & ...
%             (t_ref <= T_base + x_window_days);
% t_window  = t_ref(time_mask);
% 
% % 配色
% color_list = lines(nSats-1); % 每颗卫星不同颜色
% 
% % ========== 新增：准备保存的数据结构 ==========
% plotData = struct();
% plotData.T_base = T_base;  % 基准时间
% plotData.x_window_min = x_window_min;  % 窗口大小（分钟）
% plotData.time_relative_minutes = (t_window - T_base) * 24 * 60;  % 相对时间（分钟）
% plotData.time_absolute_MJD = t_window;  % 绝对时间（MJD）
% plotData.nSats = nSats;
% plotData.color_list = color_list;
% 
% % 为每颗卫星预分配距离数据
% plotData.distances = zeros(length(t_window), nSats-1);
% plotData.SID_list = 2:nSats;
% % ================================================
% 
% % 创建图
% fig = figure('Visible','off'); % 不显示（适合Linux非GUI环境）
% hold on;
% grid on;
% 
% for sid = 2:nSats
%     % 读取整个任务轨迹
%     sidFolder = sprintf('S%010d', sid);
%     filePath  = fullfile(basePath, sidFolder, 'outcart.dat');
%     fid = fopen(filePath, 'r');
%     rawData = textscan(fid, '%f%f%f%f%f%f%f', ...
%                        'Delimiter', ',', ...
%                        'CommentStyle', '#');
%     fclose(fid);
% 
%     pos_cur   = [rawData{2}, rawData{3}, rawData{4}];
%     pos_cur_win = pos_cur(time_mask, :);
%     pos_ref_win = pos_ref(time_mask, :);
% 
%     % 计算相对距离
%     diffVec = pos_cur_win - pos_ref_win;
%     distVec = sqrt(sum(diffVec.^2, 2));
% 
%     % ========== 新增：保存距离数据到矩阵 ==========
%     plotData.distances(:, sid-1) = distVec;
%     % ================================================
% 
%     % 绘制空心圆连线
%     plot(plotData.time_relative_minutes, distVec, ...
%         '-o', ...
%         'Color', color_list(sid-1,:), ...
%         'MarkerSize', 6, ...
%         'MarkerFaceColor', 'w', ...
%         'MarkerEdgeColor', color_list(sid-1,:), ...
%         'DisplayName', sprintf('SID%d', sid));  % 添加图例名称
% end
% 
% xlabel('时间差相对于T (分钟)');
% ylabel('距离 (km)');
% title(sprintf('相对距离变化窗口 T=%.9f MJD', T_base));
% legend('Location', 'best');
% 
% % ========== 新增：保存 .fig 文件（可在 Windows MATLAB 中编辑）==========
% outFigFile = fullfile(basePath, 'distance_variation_plot.fig');
% savefig(fig, outFigFile);
% fprintf('\n✅ 已保存 MATLAB 图形文件到: %s \n', outFigFile);
% % ====================================================================
% 
% % 保存 PNG 图片
% outPngPath = fullfile(basePath, 'distance_variation_plot.png');
% print(fig, outPngPath, '-dpng', '-r300');
% fprintf('✅ 已保存 PNG 图片到: %s \n', outPngPath);
% 
% close(fig);  % 关闭图形对象
% 
% % ========== 新增：保存数据矩阵到 .mat 文件 ==========
% outMatFile = fullfile(basePath, 'distance_variation_data.mat');
% save(outMatFile, 'plotData', 'minDist', 'minTime', 't_ref', 'pos_ref', '-v7.3');
% fprintf('✅ 已保存数据矩阵到: %s \n', outMatFile);
% % ====================================================
% 
% fprintf('\n 所有文件已保存完成！');

